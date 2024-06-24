package vyxal

import Helpers.*
import Helpers.given

import scala.collection.{mutable => mut}

object Elements {

  /** Get an element by its name
    */
  def getElement(name: String): DirectFn = Impls.elements(name)

  private object Impls {

    /** Maps element names to their implementations. This is built up as
      * implementations are defined.
      */
    val elements = mut.Map[String, DirectFn]()

    def addNilad(name: String)(impl: Context ?=> VAny): Unit = {
      elements += name -> DirectFn(() => ctx ?=> ctx.push(impl(using ctx)), 1)
    }

    def addMonad(name: String)(impl: Monad): Monad = {
      elements += name -> DirectFn(
        { () => ctx ?=>
          ctx.push(impl(ctx.pop()))
        },
        1
      )
      impl
    }

    def addMonadVect(name: String)(impl: SimpleMonad) =
      addMonad(name)(vect1(impl))

    def addDyad(name: String)(impl: Dyad): Dyad = {
      elements += name -> DirectFn(
        { () => ctx ?=>
          val arg2, arg1 = ctx.pop()
          ctx.push(impl(arg1, arg2))
        },
        2
      )
      impl
    }

    def addDyadVect(name: String)(impl: SimpleDyad) = addDyad(name)(vect2(impl))

    def addTriad(name: String)(impl: Triad): Triad = {
      elements += name -> DirectFn(
        { () => ctx ?=>
          val arg3, arg2, arg1 = ctx.pop()
          ctx.push(
            impl(arg1, arg2, arg3)
          )
        },
        3
      )
      impl
    }

    def addTriadVect(name: String)(impl: SimpleTriad) =
      addTriad(name)(vect3(impl))

    def addTetrad(name: String)(impl: Tetrad): Tetrad = {
      elements += name -> DirectFn(
        { () => ctx ?=>
          val arg4, arg3, arg2, arg1 = ctx.pop()
          ctx.push(
            impl(arg1, arg2, arg3, arg4)
          )
        },
        4
      )
      impl
    }

    /** Add an element that works directly on the entire stack */
    def addDirect(name: String)(impl: Context ?=> Unit): Unit =
      elements += name -> DirectFn(() => impl, -1)

    addDirect(",") { ctx ?=>
      Helpers.vyPrint(ctx.pop())
      Helpers.vyPrint("\n")
    }

    val add = addDyad("+")(vect2 {
      case (n1: VNum, n2: VNum)     => n1 + n2
      case (s: String, n: VNum)     => s + n
      case (n: VNum, s: String)     => n.toString + s
      case (s1: String, s2: String) => s1 + s2
      case _ => throw VyOverloadError("Functions can't be added")
    })

    val halve = addMonad("½")(vect1 {
      case n: VNum => n / 2
      case s: String =>
        val half = (s.size + 1) / 2
        VList(s.substring(0, half), s.substring(half + 1))
      case f =>
        throw VyOverloadError("Sorry, you can't halve functions")
    })

    val land = addDyad("∧") { (a, b) => if (a.toBool && b.toBool) 1 else 0 }

    val lnot = addMonad("¬") { a => if (!a.toBool) 1 else 0 }

    val lor = addDyad("∨") { (a, b) => if (a.toBool || b.toBool) 1 else 0 }

    val mul = addDyad("*")(vect2 {
      case (n1: VNum, n2: VNum) => n1 * n2
      case (n: VNum, s: String) => s * n.toInt
      case (s: String, n: VNum) => s * n.toInt
      case _                    => ???
    })

    val subtract = addDyad("-")(vect2 {
      case (n1: VNum, n2: VNum)     => n1 - n2
      case (s: String, n: VNum)     => s + "-" * n.toInt
      case (n: VNum, s: String)     => "-" * n.toInt + s
      case (s1: String, s2: String) => s1.replace(s2, "")
      case _ => throw VyOverloadError("Functions can't be subtracted")
    })

    val sum = addMonad("∑") {
      case n: VNum   => n.toInt.abs.toString().map(_.asDigit).sum
      case s: String => s
      case l: VList  => l.foldl(0)((a, b) => add.norm.apply(a, b))
      case f =>
        throw VyOverloadError(
          "What's the sum of a function even supposed to be?"
        )
    }

    val exec = addMonad("†") { case x =>
      throw NotImplementedError(
        s"† is still unimplemented (tried executing on $x)"
      )
    }

    val wrapStack = addNilad("W") { ctx ?=>
      ctx.popAll()
    }

    addNilad("n") { ctx ?=> ctx.contextVar }

    addNilad("ka") { "abcdefghijklmnopqrstuvwxyz" }
  }
}
