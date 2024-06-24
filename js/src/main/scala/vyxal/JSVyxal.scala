package vyxal

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExportTopLevel, JSExport}
import org.scalajs.dom
import org.scalajs.dom.{document, window}

/** A bridge between the interpreter and JS
  */
@JSExportTopLevel("Vyxal")
object JSVyxal {

  /** A dictionary containing docs for all the elements
    */
  @JSExport
  val elements: js.Dictionary[JSElementDocs] =
    Docs.elements.view
      .mapValues {
        case ElementDocs(
              name,
              desc,
              arity,
              overloads,
              vectorises,
              modifiesStack
            ) =>
          JSElementDocs(
            name,
            desc,
            arity,
            overloads.toJSDictionary,
            vectorises,
            modifiesStack
          )
      }
      .toMap
      .toJSDictionary

  @JSExport
  def execute(
      code: String,
      flags: String,
      inputs: String,
      outFn: js.Function1[String, Unit],
      warnFn: js.Function1[String, Unit],
      errFn: js.Function1[String, Unit]
  ): Unit = {
    given Backend with {
      override def print(out: String) = outFn(out)
      override def warn(warning: String) = warnFn(warning)
      override def err(error: String) = errFn(error)
    }

    Interpreter.execute(
      code,
      inputs.split("\n").toList,
      flags.map(_.toString).toList
    )
  }

  /** A JS-friendly version of `ElementDocs`
    */
  class JSElementDocs(
      val name: String,
      val desc: String,
      val arity: Int,
      val overloads: js.Dictionary[String],
      val vectorises: Boolean = false,
      val modifiesStack: Boolean = false
  ) extends js.Object
}
