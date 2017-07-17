package vlthr.tee.filters
import scala.util.{Try, Success, Failure}
import vlthr.tee.parser.Liquid
import vlthr.tee.core._
import vlthr.tee.util._
import vlthr.tee.core.Error._
import com.fasterxml.jackson.databind.ObjectMapper

object Filter {
  def byName(s: String): Constructor = registry.get(s).getOrElse(sp => new NoFilter()(sp))
  type Constructor = SourcePosition => Filter
  var registry: scala.collection.mutable.Map[String, Constructor] = scala.collection.mutable.Map(
    "split" -> (sp => Split()(sp)),
    "size" -> (sp => Size()(sp)),
    "json" -> (sp => Json()(sp)),
    "first" -> (sp => First()(sp)),
    "reverse" -> (sp => Reverse()(sp))
  )
  def register(name: String, f: Constructor): Unit = registry.put(name, f)
}

abstract trait NoArgs { self: Filter =>
  def checkArgs(args: List[Value]): List[Error] = {
    val correctNumberOfArgs = args.size == 0
    if (!correctNumberOfArgs) List(InvalidFilterArgs(this, args))
    else Nil
  }
  def apply(input: InputType)(
    implicit evalContext: EvalContext, parent: FilterExpr): Try[Value]
  final def apply(input: InputType, args: List[Value])(
    implicit evalContext: EvalContext, parent: FilterExpr): Try[Value] = apply(input)
}

abstract trait InputType[I <: Value] { self: Filter =>
  override def checkInput(input: Value): List[Error] = {
    if (!input.isInstanceOf[I]) InvalidFilterInput(this, input) :: Nil
    else Nil
  }
  type InputType = I
}

abstract trait FixedArgs(types: List[ValueType]) { self: Filter =>
  // TODO: A macro could generate typesafe getters for each of the expected arguments.
  def checkArgs(args: List[Value]): List[Error] = {
    val correctArgTypes = args.zip(types).forall{ case (v, expected) => v.valueType == expected }
    val correctNumberOfArgs = args.size == types.size
    if (!correctArgTypes || !correctNumberOfArgs) List(InvalidFilterArgs(this, args))
    else Nil
  }
}

abstract trait SingleArg[T <: Value] { self: Filter =>
  type ArgType = T
  def checkArgs(args: List[Value]): List[Error] = {
    val correctNumberOfArgs = args.size == 1
    val correctArgTypes = if (correctNumberOfArgs) args(0).isInstanceOf[T] else false
    if (!correctArgTypes || !correctNumberOfArgs) List(InvalidFilterArgs(this, args))
    else Nil
  }
  def applySingle(input: InputType, arg: ArgType): Try[Value]
  def apply(input: InputType, args: List[Value])(
    implicit evalContext: EvalContext, parent: FilterExpr) = applySingle(input, args(0).asInstanceOf[ArgType])
}

case class NoFilter()(implicit val sourcePosition: SourcePosition) extends Filter() {
  def name = "NoFilter"
  def apply(input: InputType, args: List[Value])(
      implicit evalContext: EvalContext, parent: FilterExpr) = ???
  def checkInput(v: Value) = ???
  def checkArgs(v: List[Value]) = ???
}

case class Split()(implicit val sourcePosition: SourcePosition) extends Filter with InputType[StringValue] with SingleArg[StringValue] {
  def name = "split"
  def applySingle(input: InputType, arg: ArgType): Try[Value] = {
    Try(Value.create(input.v.split(arg.v).toList))
  }
}

case class Json()(implicit override val sourcePosition: SourcePosition) extends Filter with InputType with NoArgs {
  def name = "json"
  override def apply(input: InputType)(
    implicit evalContext: EvalContext, parent: FilterExpr) = Success(StringValue(new ObjectMapper().writeValueAsString(Util.asJava(input.asInstanceOf[ListValue]))))
}

case class Size()(implicit val sourcePosition: SourcePosition) extends Filter with InputType[StringValue | ListValue] with NoArgs {
  def name = "size"
  override def apply(input: InputType)(
    implicit evalContext: EvalContext, parent: FilterExpr) = Try(input match {
                                                                   case ListValue(l) => IntValue(l.size)
                                                                   case StringValue(s) => IntValue(s.size)
                                                                 })
}

case class First()(implicit val sourcePosition: SourcePosition) extends Filter with InputType[StringValue | ListValue] with NoArgs {
  def name = "first"
  override def apply(input: InputType)(
    implicit evalContext: EvalContext, parent: FilterExpr) = Try(input match {
                                                                   case ListValue(l) => l.head
                                                                   case StringValue(s) => StringValue(""+s.head)
                                                                 })
}

case class Reverse()(implicit val sourcePosition: SourcePosition) extends Filter with InputType[StringValue | ListValue] with NoArgs {
  def name = "reverse"
  override def apply(input: InputType)(
    implicit evalContext: EvalContext, parent: FilterExpr) ={
    Try(input match {
          case ListValue(l) => ListValue(l.reverse)
          case StringValue(s) => StringValue(s.reverse)
        })
  }
}
