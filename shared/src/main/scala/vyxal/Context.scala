package vyxal

import scala.collection.{mutable => mut}

/** @constructor
  *   Make a Context object for the current scope
  * @param vars
  *   The variables currently in scope, accessible by their names. Null values
  *   signify that the variable is nonlocal, i.e., it should be gotten from the
  *   parent context
  * @param inputs
  *   The inputs available in this scope
  * @param parent
  *   The context inside which this context is (to inherit variables). `None`
  *   for toplevel contexts
  * @param defaultValue
  *   The default value if popping off an empty stack or accessing an undefined
  *   variable. 0 by default
  */
class Context private (
    initStack: Seq[VAny],
    val contextVar: VAny = VNum(0),
    private val vars: mut.Map[String, VAny | Null] = mut.Map(),
    val inputs: List[List[VAny]] = List(),
    private val parent: Option[Context] = None,
    val settings: Settings = Settings()
)(using backend: Backend) {
  private var stack = mut.ArrayBuffer(initStack*)
  private var printed = false

  def peek: VAny = stack(stack.size - 1)

  def pop(): VAny = if (stack.isEmpty) VNum(0) else stack.remove(stack.size - 1)

  def push(item: VAny): Unit = stack += item

  /** The last n items on the stack, in reverse order e.g. Popping 3 of
    * [1,2,3,4] results in [4,3,2]
    */
  def pop(n: Int): List[VAny] = List.fill(n)(stack.remove(stack.size - 1))

  def isStackEmpty: Boolean = stack.isEmpty

  def print(obj: Any) = {
    this.printed = true
    this.backend.print(obj.toString)
  }

  def println(obj: Any) = {
    this.print(obj)
    this.print("\n")
  }

  /** Get a variable given its name. If not found in the current context, looks
    * in parent context
    * @param default
    *   What to return if the variable doesn't exist
    */
  def getVar(
      varName: String,
      default: VAny = this.settings.defaultValue
  ): VAny =
    if (vars.contains(varName)) {
      this.vars(varName) match {
        case null => parent.get.getVar(varName, default)
        case value => value
      }
    } else {
      parent.fold(default)(_.getVar(varName, default))
    }

  /** Set a variable to a given value. If found in this context, changes its
    * value. If it's not found in the current context but it exists in the
    * parent context, sets it there. Otherwise, creates a new variable
    */
  def setVar(varName: String, value: VAny): Unit = {
    if (vars.contains(varName) && this.vars(varName) == null) {
      parent.get.setVar(varName, value)
    } else {
      this.vars(varName) = value
    }
  }

  /** Create a Context within the current context (used in structures) */
  def createChild(
      stack: Seq[VAny] = this.stack.toSeq,
      contextVar: VAny = this.contextVar,
      inputs: List[List[VAny]] = this.inputs
  ) = new Context(
    stack,
    contextVar,
    mut.Map(vars.toSeq*),
    inputs,
    Some(this),
    settings
  )

  /** Create a Context with the same attributes (useful for functions) */
  def copy(
      stack: Seq[VAny] = this.stack.toSeq,
      contextVar: VAny = this.contextVar,
      inputs: List[List[VAny]] = this.inputs
  ) = new Context(
    initStack = stack,
    contextVar = contextVar,
    vars = mut.Map(this.vars.toSeq*),
    inputs = inputs,
    parent = this.parent,
    settings = this.settings
  )

  override def toString = s"Context(stack=${stack.mkString("[", ", ", "]")})"
}

object Context {

  /** @param initStack
    *   Pre-initialized stack to use
    * @param inputs
    *   Inputs available in this scope
    * @param settings
    *   Settings set by flags
    */
  def apply(
      initStack: Seq[VAny] = Seq(),
      inputs: List[List[VAny]] = List(),
      settings: Settings = Settings()
  )(using Backend): Context =
    new Context(initStack, inputs = inputs, settings = settings)

  /** Helper to grab stack from implicit Context */
  def stack(using ctx: Context) = ctx.stack
}
