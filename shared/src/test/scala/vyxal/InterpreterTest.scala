package vyxal

import org.scalatest.flatspec.AnyFlatSpec

class InterpreterTest extends AnyFlatSpec {
  "random stuff" should "execute properly" in {
    val parsed = Parser.parse(raw"""
      3 2 +
      """).contents
    given ctx: Context = Context()
    Interpreter.execute(parsed)
    val top = ctx.pop()
    assert(top == VNum(5))
  }
}
