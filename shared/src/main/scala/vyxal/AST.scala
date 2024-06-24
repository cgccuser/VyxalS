package vyxal

import org.w3c.dom.Text

/** An AST representing Vyxal code
  */
sealed trait AST {
  def textRange: TextRange
}

/** Represents the position of a character */
case class Pos(row: Int, col: Int) {
  override def toString = s"$row:$col"
}

/** Represents the space a chunk of text occupies */
case class TextRange(start: Pos, end: Pos) {
  override def toString = s"$start to $end"
}

object TextRange {

  /** For nodes that were created programmatically, not parsed */
  def synthetic = TextRange(Pos(0, 0), Pos(0, 0))
}

case class Literal(value: VAny, textRange: TextRange) extends AST

case class Element(symbol: String, textRange: TextRange) extends AST

/** Groups multiple elements together
  */
case class Cmds private(cmds: AST*) extends AST {
  override def textRange =
    if (cmds.isEmpty) TextRange.synthetic
    else TextRange(cmds.head.textRange.start, cmds.last.textRange.end)
  
  override def equals(other: Any) = other match {
    case Cmds(otherCmds*) => cmds.sameElements(otherCmds)
    case _ => false
  }
}

object Cmds {
  def apply(cmds: AST*) = new Cmds(cmds.toList*)
}

object Empty extends AST {
  def textRange = TextRange.synthetic
}

/** Represents variable access (←varName)
  */
case class VarGet(name: String, textRange: TextRange) extends AST

/** Represents variable assignment (→varName)
  */
case class VarSet(name: String, textRange: TextRange) extends AST

/** Execute the top of the stack (or just run element †) */
case class ExecFn(textRange: TextRange) extends AST

case class If(
    truthy: AST,
    falsey: Option[AST],
    textRange: TextRange
) extends AST

/** A for loop
  * @param loopVar
  *   The name of the variable to loop over
  * @param body
  *   The body of the loop
  */
case class For(
    loopVar: Option[String],
    body: AST,
    textRange: TextRange
) extends AST

/** @param cond
  *   The code to run to test each time
  * @param body
  *   The code to run if the condition is true
  */
case class While(
    cond: Option[AST],
    body: AST,
    textRange: TextRange
) extends AST

case class FnDef(
    name: String,
    arityOrParams: List[Int | String],
    body: AST,
    textRange: TextRange
) extends AST {
  def arity: Int = arityOrParams match {
    case List(arity: Int) => arity
    case ps               => 1.max(ps.count(_.isInstanceOf[String]))
  }
}

case class Lambda(
    body: AST,
    kind: LambdaKind = LambdaKind.Normal,
    textRange: TextRange
) extends AST

enum LambdaKind {
  case Normal, OneElement, TwoElement, ThreeElement, FourElement
}

/** A lambda after which an element runs (map, filter, and sort lambdas)
  */
case class LambdaWithOp(
    lam: Lambda,
    after: AST
) extends AST {
  override def textRange = lam.textRange
}

enum Modifier(symbol: String) extends AST {
  case ConditionalExecute(body: AST, textRange: TextRange) extends Modifier("¿")
  case ApplyToEachStackItem(
      body: AST,
      textRange: TextRange
  ) extends Modifier("æ")
}
