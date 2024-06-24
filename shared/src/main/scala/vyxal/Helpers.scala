package vyxal

import spire.math.Number

object Helpers {
  def vyPrint(elem: VAny)(using ctx: Context): Unit = {
    elem match {
      case lst: VList => lst.output()
      case top        => ctx.print(top.toString)
    }
  }

  def range(fromInc: VNum, toExc: VNum): VList = {
    ???
  }

  extension (any: VAny) {
    def toBool: Boolean = any match {
      case num: VNum   => num != 0
      case str: String => str.nonEmpty
      case lst: VList  => lst.nonEmpty
      case fun: VFun   => true
    }

    def toList(using ctx: Context): VList = any match {
      case num: VNum   => ctx.settings.numToList(num)
      case str: String => VList(str.map(_.toString)*)
      case lst: VList  => lst
      case fun: VFun   => VList(fun)
    }
  }
}
