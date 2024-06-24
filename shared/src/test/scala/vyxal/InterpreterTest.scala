package vyxal

import org.scalatest.funspec.AnyFunSpec

class InterpreterTest extends AnyFunSpec {
  given Backend with {}

  inline def runTest(code: String, expected: VAny) =
    it("should return the right result for " + code) {
      val ctx = Interpreter.execute(code, flags=List("O"))
      assert(ctx.pop() == expected)
    }
  
  describe("+ and -") {
    runTest("3 2 +", 5)
    runTest("3 2 -", 1)
  }

  describe("triple function") {
    runTest("""
      @triple|3 *}
      4 ←triple†
      """, 12)
  }

  describe("conditional execute modifier") {
    runTest("1 3 2 +¿", 4)
    runTest("1 3 0 +¿", 3)
  }

  describe("ternary if modifier") {
    runTest("4 2 3]", 2)
    runTest("5 3 0 +-]", 2)
    runTest("5 3 9 +-]", 8)
  }

  describe("apply to each stack item modifier") {
    runTest("2 4 6 8 ½æ W", VList(1, 2, 3, 4))

    runTest("5 1 7 2 4 2+¢æ W", VList(7, 3, 9, 4, 6))

    // TODO: currently modifiers that take a lambda don't work
  }

  describe("lambda") {
    runTest("2 λ1 + 3} †", 3)
    // Ensure only one value gets returned from the lambda
    runTest("2 λ1 + 4} † W", VList(4))
  }

  // TODO: auto-generate some of these tests from the YAML
}
