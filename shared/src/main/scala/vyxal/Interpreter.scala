package vyxal

import vyxal.Helpers.{toBool, toList}

object Interpreter {
  def execute(
      code: String,
      inputs: List[VAny] = List.empty,
      flags: List[String] = List.empty
  )(using Backend): Context = {
    val ast = Parser.parse(code)
    given ctx: Context = Context()
    execute(ast)
    if (ctx.settings.implicitOutput && !ctx.isStackEmpty) {
      ctx.println(ctx.peek)
    }
    ctx
  }

  /** Execute a single line of code in a REPL */
  def executeLine(
      line: String
  )(using ctx: Context): Context = {
    val ast = Parser.parse(line)
    execute(ast)
    if (ctx.settings.implicitOutput && !ctx.isStackEmpty) {
      ctx.println(ctx.peek)
    }
    ctx
  }

  def execute(ast: AST)(using ctx: Context): Unit = {
    try {
      ast match {
        case Literal(value, _) => ctx.push(value)
        case l: Lambda         => ctx.push(VFun.Lam(l, 1, ctx.createChild()))
        case Element(name, _)  => Elements.getElement(name)()
        case m: Modifier       => executeModified(m)
        case Cmds(cmds*)       => cmds.foreach(execute)
        case LambdaWithOp(lam, after) =>
          execute(lam)
          execute(after)
        case VarGet(varName, _) =>
          ctx.push(ctx.getVar(varName))
        case VarSet(varName, _) => ctx.setVar(varName, ctx.pop())
        case fn: FnDef =>
          ctx.setVar(fn.name, VFun.FnRef(fn, ctx.createChild()))
        case If(truthy, falsey, _) =>
          if (ctx.pop().toBool) execute(truthy)
          else falsey.map(execute)
        case While(cond, body, _) =>
          cond match {
            case Some(condAst) =>
              while {
                execute(condAst)
                ctx.pop().toBool
              } do execute(body)
            case None =>
              while (true) execute(body)
          }
        case For(loopVar, body, _) =>
          val elems = ctx.pop().toList
          for (elem <- elems) {
            given newCtx: Context = ctx.createChild(contextVar = elem)
            loopVar.map { varName => newCtx.setVar(varName, elem) }
            execute(body)
          }
        case ExecFn(_) =>
          ctx.pop() match {
            case vf: VFun => executeFn(vf)
            case _        => Elements.getElement("†")()
          }
      }
    } catch {
      case e: Exception =>
        throw Exception(
          s"Errored while executing element $ast, ctx=$ctx, e=${e.getMessage}",
          e
        )
      case re: RuntimeException =>
        throw RuntimeException(
          s"Errored while executing element $ast, ctx=$ctx",
          re
        )
      case e: Error =>
        throw Error(
          s"Errored while executing element $ast, ctx=$ctx, e=${e.getMessage}",
          e
        )
    }
  }

  private def executeModified(mod: Modifier)(using ctx: Context) = mod match {
    case Modifier.ConditionalExecute(body, _) =>
      if (ctx.pop.toBool) {
        execute(body)
      }
    case Modifier.ApplyToEachStackItem(body, _) =>
      val stack = ctx.popAll()
      stack.foreach { elem =>
        ctx.push(elem)
        execute(body)
      }
  }

  private def executeFn(vf: VFun)(using ctx: Context) = {
    val newCtx = Context.fnCallCtx(vf.ctx, ctx)

    vf match {
      case VFun.FnRef(fn, _) =>
        fn.arityOrParams match {
          case List(arity: Int) =>
            execute(fn.body)(using newCtx)
          case List() =>
            // Assume arity 1
            newCtx.push(ctx.pop())
            execute(fn.body)(using newCtx)
          case _ =>
            for (param <- fn.arityOrParams) {
              param match {
                case s: String =>
                  newCtx.push(ctx.pop())
                  execute(VarSet(s, TextRange.synthetic))(using newCtx)
                case n: Int => newCtx.push(n.vnum)
              }
            }
            execute(fn.body)(using newCtx)
        }
      case VFun.Lam(lam, arity, _) =>
        for (_ <- 1.to(arity)) newCtx.push(ctx.pop())
        execute(lam.body)(using newCtx)
    }

    if (!newCtx.isStackEmpty) {
      ctx.push(newCtx.pop())
    }
  }
}
