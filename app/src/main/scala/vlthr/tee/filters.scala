package vlthr.tee.filters
import scala.util.{Try, Success, Failure}
import vlthr.tee.parser.Liquid
import vlthr.tee.core._
import vlthr.tee.util._
import vlthr.tee.core.Error._
import com.fasterxml.jackson.databind.ObjectMapper

object Filter {
  def byName(s: String): Constructor = registry.get(s).getOrElse((args, sp) => NoFilter()(sp))
  type Constructor = (List[Value], SourcePosition) => Filter
  var registry: scala.collection.mutable.Map[String, Constructor] = scala.collection.mutable.Map(
    "split" -> ((args, sp) => Split(args)(sp)),
    "join" -> ((args, sp) => Join(args)(sp)),
    "size" -> ((args, sp) => Size(args)(sp)),
    "json" -> ((args, sp) => Json(args)(sp)),
    "first" -> ((args, sp) => First(args)(sp)),
    "last" -> ((args, sp) => Last(args)(sp)),
    "append" -> ((args, sp) => Append(args)(sp)),
    "capitalize" -> ((args, sp) => Capitalize(args)(sp)),
    "downcase" -> ((args, sp) => Downcase(args)(sp)),
    "upcase" -> ((args, sp) => Upcase(args)(sp)),
    "escape" -> ((args, sp) => Escape(args)(sp)),
    "reverse" -> ((args, sp) => Reverse(args)(sp))
  )
  def register(name: String, f: Constructor): Unit = registry.put(name, f)
}

abstract trait NoArgs { self: Filter =>
  def checkArgs(args: List[Value]): List[Error] = {
    val correctNumberOfArgs = args.size == 0
    if (!correctNumberOfArgs) List(InvalidFilterArgs(this, args))
    else Nil
  }
  def apply(input: Value)(
    implicit evalContext: EvalContext, parent: FilterExpr): Try[Value]
  final def apply(input: Value, args: List[Value])(
    implicit evalContext: EvalContext, parent: FilterExpr): Try[Value] = apply(input)
}

abstract trait InputType(t: ValueType) { self: Filter =>
  override def checkInput(input: Value): List[Error] = {
    if (!t.matches(input)) InvalidFilterInput(this, input) :: Nil
    else Nil
  }
}

abstract trait FixedArgs(types: List[ValueType]) { self: Filter =>
  // TODO: A macro could generate typesafe getters for each of the expected arguments.
  def checkArgs(args: List[Value]): List[Error] = {
    val correctArgTypes = args.zip(types).forall{ case (v, expected) => expected.matches(v) }
    val correctNumberOfArgs = args.size == types.size
    if (!correctArgTypes || !correctNumberOfArgs) List(InvalidFilterArgs(this, args))
    else Nil
  }
}

abstract trait SingleArg(expected: ValueType) { self: Filter =>
  def checkArgs(args: List[Value]): List[Error] = {
    val correctNumberOfArgs = args.size == 1
    val correctArgTypes = if (correctNumberOfArgs) expected.matches(args(0)) else false
    if (!correctArgTypes || !correctNumberOfArgs) List(InvalidFilterArgs(this, args))
    else Nil
  }
}

case class NoFilter()(implicit val sourcePosition: SourcePosition) extends Filter(Nil) {
  def name = "NoFilter"
  def apply(input: Value, args: List[Value])(
      implicit evalContext: EvalContext, parent: FilterExpr) = ???
  def checkInput(v: Value) = ???
  def checkArgs(v: List[Value]) = ???
}

case class Split(args: List[Value])(implicit val sourcePosition: SourcePosition) extends Filter(args) with InputType(ValueType.String) with SingleArg(ValueType.String) {
  def name = "split"
  override def filter(input: StringValue)(
    implicit evalContext: EvalContext, parent: FilterExpr) = {
    val pattern = args(0).asInstanceOf[StringValue]
    Try(Value.create(input.v.split(pattern.v).toList))
  }
}

case class Json(args: List[Value])(implicit override val sourcePosition: SourcePosition) extends Filter(args) with InputType(ValueType.List) with NoArgs {
  def name = "json"
  override def filter(input: ListValue)(
    implicit evalContext: EvalContext, parent: FilterExpr) = Success(StringValue(new ObjectMapper().writeValueAsString(Util.asJava(input.asInstanceOf[ListValue]))))
}

case class Size(args: List[Value])(implicit val sourcePosition: SourcePosition) extends Filter(args) with InputType(ValueType.List | ValueType.String) with NoArgs {
  def name = "size"
  override def filter(input: ListValue)(
    implicit evalContext: EvalContext, parent: FilterExpr) = Try(IntValue(input.v.size))
  override def filter(input: StringValue)(
    implicit evalContext: EvalContext, parent: FilterExpr) = Try(IntValue(input.v.size))
}

case class First(args: List[Value])(implicit val sourcePosition: SourcePosition) extends Filter(args) with InputType(ValueType.List | ValueType.String) with NoArgs {
  def name = "first"
  override def filter(input: StringValue)(
    implicit evalContext: EvalContext, parent: FilterExpr) = Try(StringValue(""+input.v.head))
  override def filter(input: ListValue)(
    implicit evalContext: EvalContext, parent: FilterExpr) = Try(input.v.head)
}

case class Last(args: List[Value])(implicit val sourcePosition: SourcePosition) extends Filter(args) with InputType(ValueType.List | ValueType.String) with NoArgs {
  def name = "last"
  override def filter(input: StringValue)(
    implicit evalContext: EvalContext, parent: FilterExpr) = Try(StringValue(""+input.v.last))
  override def filter(input: ListValue)(
    implicit evalContext: EvalContext, parent: FilterExpr) = Try(input.v.last)
}

case class Reverse(args: List[Value])(implicit val sourcePosition: SourcePosition) extends Filter(args) with InputType(ValueType.List | ValueType.String) with NoArgs {
  def name = "reverse"
  override def filter(input: StringValue)(
    implicit evalContext: EvalContext, parent: FilterExpr) = Try(StringValue(input.v.reverse))
  override def filter(input: ListValue)(
    implicit evalContext: EvalContext, parent: FilterExpr) = Try(ListValue(input.v.reverse))
}

case class Join(args: List[Value])(implicit val sourcePosition: SourcePosition) extends Filter(args) with InputType(ValueType.List) with SingleArg(ValueType.String) {
  def name = "join"
  override def filter(input: ListValue)(
    implicit evalContext: EvalContext, parent: FilterExpr) = {
    val delim = args(0).asInstanceOf[StringValue]
    Try(StringValue(input.v.map(_.render.get).mkString(delim.v)))
  }
}

case class Capitalize(args: List[Value])(implicit val sourcePosition: SourcePosition) extends Filter(args) with InputType(ValueType.String) with NoArgs {
  def name = "capitalize"
  override def filter(input: StringValue)(
    implicit evalContext: EvalContext, parent: FilterExpr) = {
    Try(StringValue(Character.toUpperCase(input.v(0)) + input.v.substring(1)))
  }
}

case class Downcase(args: List[Value])(implicit val sourcePosition: SourcePosition) extends Filter(args) with InputType(ValueType.String) with NoArgs {
  def name = "downcase"
  override def filter(input: StringValue)(
    implicit evalContext: EvalContext, parent: FilterExpr) = {
    Try(StringValue(input.v.map(c => Character.toLowerCase(c)).mkString))
  }
}

case class Upcase(args: List[Value])(implicit val sourcePosition: SourcePosition) extends Filter(args) with InputType(ValueType.String) with NoArgs {
  def name = "upcase"
  override def filter(input: StringValue)(
    implicit evalContext: EvalContext, parent: FilterExpr) = {
    Try(StringValue(input.v.map(c => Character.toUpperCase(c)).mkString))
  }
}

case class Append(args: List[Value])(implicit val sourcePosition: SourcePosition) extends Filter(args) with InputType(ValueType.String) with SingleArg(ValueType.String) {
  def name = "append"
  override def filter(input: StringValue)(
    implicit evalContext: EvalContext, parent: FilterExpr) = {
    val end = args(0).asInstanceOf[StringValue]
    Try(StringValue(input.v + end.v))
  }
}

case class Escape(args: List[Value])(implicit val sourcePosition: SourcePosition) extends Filter(args) with InputType(ValueType.String) with NoArgs {
  def name = "escape"
  override def filter(input: StringValue)(
    implicit evalContext: EvalContext, parent: FilterExpr) = {
    Try(StringValue(input.v
                      .replace("<", "&lt;")
                      .replace(">", "&gt;")
                      .replace("\"", "&quot;")
                      .replace("&", "&amp;")))
  }
}
