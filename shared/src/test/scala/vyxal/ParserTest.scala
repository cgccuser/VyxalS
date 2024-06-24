package vyxal

import org.scalatest.flatspec.AnyFlatSpec

class ParserTest extends AnyFlatSpec {
  given Backend = new Backend {}

  def test(code: String, expected: AST) = {
    code should "parse correctly" in {
      val parsed = Parser.parse(code)
      assertResult(expected)(parsed)
    }
  }

  /** Just a helper to make creating `TextRange`s less verbose */
  def TR(startRow: Int, startCol: Int, endRow: Int, endCol: Int): TextRange =
    TextRange(Pos(startRow, startCol), Pos(endRow, endCol))
  
  /** A `TextRange` for a node that's on the first row */
  def TR(startCol: Int, endCol: Int): TextRange =
    TR(0, startCol, 0, endCol)

  test(
    "(foo | bar} [ a+ | a324 } { 3",
    Cmds(
      For(Some("foo"), Cmds(Element("b", TR(7, 8)), Element("a", TR(8, 9)), Element("r", TR(9, 10))), TR(0, 11)),
      If(
        Cmds(Element("a", TR(14, 15)), Element("+", TR(15, 16))),
        Some(Cmds(Element("a", TR(19, 20)), Literal(324, TR(20, 23)))),
        TR(12, 25)
      ),
      While(
        None,
        Literal(3, TR(28, 29)),
        TR(26, 29)
      )
    )
  )

  // test(
  //   "+¤",
  //   Lambda(Element("+"), LambdaKind.OneElement)
  // )

  // test(
  //   "+-¢",
  //   Lambda(Cmds(Element("+"), Element("-")), LambdaKind.TwoElement)
  // )

  // test(
  //   "+¤+¢¤",
  //   Lambda(
  //     Cmds(Element("+"), Element("+")),
  //     LambdaKind.OneElement
  //   )
  // )

  // test(
  //   "λ1 2 +}",
  //   Lambda(Cmds(Literal(1), Literal(2), Element("+")))
  // )
}
