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

abstract trait NoArgs extends Filter {
  def checkArgs(v: List[Value]): List[Error] = v match {
    case Nil => Nil
    case _ => ???
  }
}

abstract trait InputType(t: ValueType) extends Filter {
  def checkInput(input: Value): List[Error] = {
    if (input.valueType != t) ??? // Return error
    else Nil
  }
}

abstract trait FixedArgs(types: List[ValueType]) extends Filter {
  // TODO: A macro could generate typesafe getters for each of the expected arguments.

  def checkArgs(args: List[Value]): List[Error] = {
    // If wrong number of args, return error
    // For each arg, its type should match the corresponding ValueType in the types list
    List()
  }
}

// abstract trait SingleArg[T :< Value](t: ValueType) extends FixedArgs(t :: Nil) {
//   override def checkArgs(args: List[Value]) = {
//     super.checkArgs ++ args
//   }
//   def arg: T = args(0)

abstract trait PartialFilter extends Filter with NoArgs {
  def definedForIntInput: Boolean = false
  def definedForBooleanInput: Boolean = false
  def definedForStringInput: Boolean = false
  def definedForListInput: Boolean = false
  def definedForMapInput: Boolean = false
  def isDefinedForInput(v: Value): Boolean = v match {
    case IntValue(_) => definedForIntInput
    case BooleanValue(_) => definedForBooleanInput
    case StringValue(_) => definedForStringInput
    case MapValue(_) => definedForMapInput
    case ListValue(_) => definedForListInput
  }
  def transform(value: IntValue): Value = ???
  def transform(value: BooleanValue): Value = ???
  def transform(value: StringValue): Value = ???
  def transform(value: ListValue): Value = ???
  def transform(value: MapValue): Value = ???

  def apply(input: Value, args: List[Value])(
    implicit evalContext: EvalContext, parent: FilterExpr) = (input, args) match {
    case (v@IntValue(_), Nil) => Try(transform(v))
    case (v@BooleanValue(_), Nil) => Try(transform(v))
    case (v@StringValue(_), Nil) => Try(transform(v))
    case (v@MapValue(_), Nil) => Try(transform(v))
    case (v@ListValue(_), Nil) => Try(transform(v))
    case _ =>
      fail(FilterApplicationError(parent, this, input, args))
  }
}
abstract trait StringFilter extends PartialFilter {
  override def definedForStringInput: Boolean = true
}
abstract trait ListFilter extends PartialFilter {
  override def definedForListInput: Boolean = true
}

case class NoFilter()(implicit sp: SourcePosition) extends Filter()(sp) {
  def name = "NoFilter"
  def apply(input: Value, args: List[Value])(
      implicit evalContext: EvalContext, parent: FilterExpr) = Success(input)
  def isDefinedForInput(v: Value): Boolean = true
  def isDefinedForArgs(v: List[Value]): Boolean = true
}

case class Split()(implicit sp: SourcePosition) extends Filter()(sp) {
  def name = "split"
  def isDefinedForInput(v: Value): Boolean = v match {
    case StringValue(_) => true
    case _ => false
  }
  def isDefinedForArgs(v: List[Value]): Boolean = v match {
    case StringValue(_) :: Nil => true
    case _ => false
  }
  def apply(input: Value, args: List[Value])(
      implicit evalContext: EvalContext, parent: FilterExpr) = (input, args) match {
    case (StringValue(input), StringValue(pattern) :: Nil) =>
      Try(Value.create(input.split(pattern).toList))
    case _ =>
      fail(FilterApplicationError(parent, this, input, args))
  }
}

case class Json()(implicit sp: SourcePosition) extends ListFilter {
  def name = "json"
  override def transform(value: ListValue) = StringValue(new ObjectMapper().writeValueAsString(Util.asJava(value)))
}

case class Size()(implicit sp: SourcePosition) extends ListFilter with StringFilter {
  def name = "size"
  override def transform(value: ListValue) = IntValue(value.v.size)
  override def transform(value: StringValue) = IntValue(value.v.size)
}

case class First()(implicit sp: SourcePosition) extends ListFilter {
  def name = "first"
  override def transform(value: ListValue) = Value.create(value.v.head)
}

case class Reverse()(implicit sp: SourcePosition) extends ListFilter with StringFilter {
  def name = "reverse"
  override def transform(seq: ListValue) = ListValue(seq.v.reverse)
  override def transform(seq: StringValue) = StringValue(seq.v.reverse)
}
