package vyxal

/** Settings that you can configure with flags
  * @param defaultValue
  *   The default value if popping off an empty stack or accessing an undefined
  *   variable. 0 by default
  * @param numToList
  *   The function used to convert numbers to lists
  */
case class Settings(
    defaultValue: VAny = VNum(0),
    numToList: VNum => VList = n => VList(VNum(1).to(n))
)