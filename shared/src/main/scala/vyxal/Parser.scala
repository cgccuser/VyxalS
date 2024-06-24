package vyxal

import scala.collection.{mutable => mut}

import spire.math.Number

class SyntaxError(msg: String, row: Int, col: Int)
    extends RuntimeException(s"Syntax error: $msg at $row:$col")

trait Popper {
  def pop(): AST
}

class Parser(private val prog: Iterator[Char]) {
  private var row, col = 0
  private val buf = mut.ListBuffer.empty[Char]

  /** Do we have any more characters to consume?
    */
  private def nonEmpty = buf.nonEmpty || prog.nonEmpty
  private def isEmpty = !nonEmpty

  private def peek = {
    if (buf.isEmpty) {
      val char = prog.next
      buf += char
    }
    buf.head
  }

  private def next() = {
    val char = if (buf.isEmpty) prog.next else buf.remove(0)
    if (char == '\n') {
      this.row += 1
      this.col = 0
    } else {
      this.col += 1
    }
    char
  }

  /** Skip over whitespace and comments
    */
  private def trim() = {
    while (this.nonEmpty && this.peek.isWhitespace) {
      if (next() == '#') {
        while (this.nonEmpty && this.peek != '\n') {
          next()
        }
      }
    }
  }

  /** Pop the last `num` ASTs and reverse them */
  private def pop(num: Int = 1)(using pp: Popper): AST = {
    val asts = Seq.fill(num) {
      val ast = pp.pop()
      if (ast.isInstanceOf[Lambda]) {
        ast.asInstanceOf[Lambda].body
      } else {
        ast
      }
    }
    if (num == 1) asts.head
    else Cmds(asts.reverse*)
  }

  /** Current position */
  private def currPos = Pos(this.row, this.col)

  /** Creates a TextRange starting from `start` to the current position */
  private def rangeFrom(start: Pos) = TextRange(start, currPos)

  /** Whether or not this character closes a structure
    */
  private def isStructureCloser(char: Char) =
    char == '}' || char == '|'

  private def isAlpha(char: Char) =
    'a' <= char && char <= 'z' || 'A' <= char && char <= 'Z'

  /** Parse a single AST. Expects the program to have been trimmed beforehand
    */
  private def parseAST()(using pp: Popper): AST = {
    assert(this.nonEmpty)

    val startPos = currPos

    val char = next()
    assert(!isStructureCloser(char))
    assert(!char.isWhitespace)
    assert(char != '#')

    char match {
      case 'λ' | 'ƛ' | 'Ω' | 'Λ' | 'µ' =>
        val body = parseElemGroup()
        // Get rid of the closing }
        if (buf.nonEmpty) this.next()
        val lam = Lambda(body, LambdaKind.Normal, rangeFrom(startPos))
        if (char == 'λ') {
          lam
        } else {
          val elem = (char: @unchecked) match {
            case 'ƛ' => "M"
            case 'Ω' => "F"
            case 'Λ' => "R"
            case 'µ' => "ṡ"
          }
          LambdaWithOp(lam, Element(elem, TextRange.synthetic))
        }
      case '†' => ExecFn(rangeFrom(startPos))
      case '[' =>
        parseCtrlStruct('}') { (truthy, falsey) =>
          If(truthy, falsey, TextRange(startPos, currPos))
        }
      case '{' =>
        parseCtrlStruct('}') {
          case (cond, Some(body)) => While(Some(cond), body, rangeFrom(startPos))
          case (body, None)       => While(None, body, rangeFrom(startPos))
        }
      case '(' =>
        parseCtrlStruct('}') {
          case (varName, Some(body)) =>
            val nameStr = varName match {
              case Element(name, _) => name
              case Cmds(cmds*) =>
                cmds.collect { case Element(name, _) => name }.mkString("")
              case _ => ""
            }
            val alphaName = nameStr.filter(c => isAlpha(c) || c == '_')
            For(Some(alphaName), body, rangeFrom(startPos))
          case (body, None) =>
            For(None, body, rangeFrom(startPos))
        }
      case '←' => VarGet(parseIdent(), rangeFrom(startPos))
      case '→' => VarSet(parseIdent(), rangeFrom(startPos))
      case '@' =>
        parseFn(startPos)
      case '.' =>
        Literal(if (this.isEmpty) (0.5).vnum else afterDecimal(0), rangeFrom(startPos))
      case d if d.isDigit =>
        var num = BigInt(d - '0')
        while (nonEmpty && this.peek.isDigit) {
          num = num * 10 + (this.next() - '0')
        }
        Literal(
          if (nonEmpty && this.peek == '.') {
            this.next()
            afterDecimal(num)
          } else {
            num.vnum
          },
          rangeFrom(startPos)
        )
      case c @ ('∆' | 'ø' | 'Þ' | '¨' | 'k') =>
        parseModifierOrElem(s"$c${this.next()}")
      case c => parseModifierOrElem(s"$c")
    }
  }

  /** Get the part of a number after the decimal as a `VNum`
    * @param beforeDecimal
    *   The stuff before the decimal. Gets added to the stuff after the decimal
    */
  private def afterDecimal(beforeDecimal: BigInt): VNum = {
    var num = beforeDecimal
    var den = BigInt(1)
    while (nonEmpty && this.peek.isDigit) {
      num = num * 10 + (next() - '0')
      den *= 10
    }
    num.vnum / den
  }

  private def parseIdent(): String = {
    val id = StringBuilder()
    while (this.nonEmpty && isAlpha(this.peek)) id += this.next()
    id.toString
  }

  /** Parse if, for, while. Assumes the first character of the structure is
    * already consumed
    * @param close
    *   The closing character of the structure
    * @param f
    *   A function to apply to the parts of the structure
    * @return
    *   The first part of the structure and the second part, if it exists
    */
  private def parseCtrlStruct(
      close: Char
  )(f: (AST, Option[AST]) => AST): AST = {
    val first = parseElemGroup()
    if (this.isEmpty) {
      f(first, None)
    } else if (this.peek == '|') {
      this.next()
      val second = parseElemGroup()
      if (this.nonEmpty && this.peek == close) this.next()
      f(first, Some(second))
    } else if (this.peek == close) {
      this.next()
      f(first, None)
    } else {
      // Some other structure closed here, so autoclose the current one
      f(first, None)
    }
  }

  private def parseFn(startPos: Pos): AST = {
    this.trim()
    val name = StringBuilder()
    while (this.nonEmpty && this.peek != ':' && this.peek != '|') {
      name += this.next()
    }
    if (this.isEmpty) {
      FnDef(name.toString, List.empty, Cmds(), rangeFrom(startPos))
    } else {
      val arityOrParams = if (this.next() == '|') {
        List.empty
      } else {
        val params = mut.ListBuffer[Int | String]()
        while (this.nonEmpty && this.peek != '|') {
          val param = StringBuilder()
          var allDigits = true
          while (this.nonEmpty && this.peek != ':' && this.peek != '|') {
            val c = this.next()
            param += c
            if (!c.isDigit) {
              allDigits = false
            }
          }
          if (this.nonEmpty) this.next()
          if (allDigits) {
            params += param.toString.toInt
          } else {
            params += param.toString
          }
        }
        if (this.nonEmpty) this.next()
        params.toList
      }
      val body = parseElemGroup()
      if (this.nonEmpty && this.peek == '}') this.next()
      FnDef(name.toString, arityOrParams, body, rangeFrom(startPos))
    }
  }

  private def parseModifierOrElem(sym: String)(using pp: Popper): AST = {
    sym match {
      case "¤" =>
        val body = pop(1)
        Lambda(body, LambdaKind.OneElement, rangeFrom(body.textRange.start))
      case "¢" =>
        val body = pop(2)
        Lambda(body, LambdaKind.TwoElement, rangeFrom(body.textRange.start))
      case "€" =>
        val body = pop(3)
        Lambda(body, LambdaKind.ThreeElement, rangeFrom(body.textRange.start))
      case "§" =>
        val body = pop(4)
        Lambda(body, LambdaKind.FourElement, rangeFrom(body.textRange.start))
      case "¿" =>
        val body = pop()
        Modifier.ConditionalExecute(body, rangeFrom(body.textRange.start))
      case "æ" =>
        val body = pop()
        Modifier.ApplyToEachStackItem(body, rangeFrom(body.textRange.start))
      case "]" =>
        // todo determine if ternary if should be its own thing
        val falsey = pop()
        val truthy = pop()
        If(truthy, Some(falsey), rangeFrom(falsey.textRange.start))
      case _ => Element(sym, rangeFrom(Pos(this.row, this.col - sym.length)))
    }
  }

  private def parseASTOrEmpty()(using pp: Popper): AST = {
    this.trim()
    if (this.nonEmpty) parseAST()
    else Cmds()
  }

  /** Parse a bunch of elements up to a structure closer and return those
    * elements as well as the last character parsed.
    */
  private def parseElems(): List[AST] = {
    val elems = mut.ListBuffer.empty[AST]
    this.trim()
    while (this.nonEmpty && !isStructureCloser(this.peek)) {
      given pp: Popper with {
        override def pop(): AST = {
          val ast = elems.last
          elems.dropRightInPlace(1)
          ast
        }
      }
      elems += parseAST()
      this.trim()
    }
    elems.toList
  }

  /** Parse multiple elements as if they're one AST
    */
  private def parseElemGroup(): AST = parseElems() match {
    case List(elem) => elem
    case elems      => Cmds(elems*)
  }
}

object Parser {
  def parse(prog: Iterator[Char]): AST = {
    val parser = Parser(prog)
    val ast = parser.parseElemGroup()
    assert(
      parser.isEmpty,
      s"Parsed ${ast}, but not empty: buf=${parser.buf.toList}, prog=${prog.toList}"
    )
    ast
  }

  def parse(str: String): AST = Parser.parse(str.iterator)
}
