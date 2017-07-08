package vlthr.tee.filters
import scala.util.{Try, Success, Failure}
import vlthr.tee.parser.Liquid
import vlthr.tee.core._
import vlthr.tee.util._
import vlthr.tee.core.Error._
import com.fasterxml.jackson.databind.ObjectMapper

object Filter {
  def byName(s: String): Filter = registry.get(s).getOrElse(NoFilter())
  var registry: scala.collection.mutable.Map[String, Filter] = scala.collection.mutable.Map(
    "split" -> Split(),
    "size" -> Size(),
    "json" -> Json(),
    "first" -> First(),
    "reverse" -> Reverse()
  )
  def register(f: Filter): Unit = registry.put(f.name, f)
}

abstract trait NoArgs extends Filter {
  def isDefinedForArgs(v: List[Value]): Boolean = v match {
    case Nil => true
    case _ => false
  }
}

case class NoFilter() extends Filter {
  def name = "NoFilter"
  def apply(input: Value, args: List[Value])(
      implicit evalContext: EvalContext, parent: FilterExpr) = Success(input)
  def isDefinedForInput(v: Value): Boolean = true
  def isDefinedForArgs(v: List[Value]): Boolean = true
}

case class Split() extends Filter {
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

case class Size() extends Filter with NoArgs {
  def name = "size"
  def isDefinedForInput(v: Value): Boolean = v match {
    case ListValue(_) => true
    case _ => false
  }
  def apply(input: Value, args: List[Value])(
      implicit evalContext: EvalContext, parent: FilterExpr) = (input, args) match {
    case (ListValue(l), Nil) => Try(IntValue(l.size))
    case _ =>
      fail(FilterApplicationError(parent, this, input, args))
  }
}

case class Json() extends Filter with NoArgs {
  def name = "json"
  def isDefinedForInput(v: Value): Boolean = true
  def apply(input: Value, args: List[Value])(
    implicit evalContext: EvalContext, parent: FilterExpr) = (input, args) match {
    case (l @ ListValue(_), Nil) => Try(new ObjectMapper().writeValueAsString(Util.asJava(l))).map(v => StringValue(v))
    case _ =>
      fail(FilterApplicationError(parent, this, input, args))
  }
}

case class First() extends Filter with NoArgs {
  def name = "first"
  def isDefinedForInput(v: Value): Boolean = v match {
    case ListValue(_) => true
    case StringValue(_) => true
    case _ => false
  }
  def apply(input: Value, args: List[Value])(
    implicit evalContext: EvalContext, parent: FilterExpr) = (input, args) match {
    case (ListValue(l), Nil) => Try(l.head)
    case (StringValue(s), Nil) => Try(s.charAt(0).toString).map(v => StringValue(v))
    case _ =>
      fail(FilterApplicationError(parent, this, input, args))
  }
}
case class Reverse() extends Filter with NoArgs {
  def name = "reverse"
  def isDefinedForInput(v: Value): Boolean = v match {
    case ListValue(_) => true
    case StringValue(_) => true
    case _ => false
  }
  def apply(input: Value, args: List[Value])(
    implicit evalContext: EvalContext, parent: FilterExpr) = (input, args) match {
    case (ListValue(l), Nil) => Try(l.reverse).map(v => ListValue(v))
    case (StringValue(s), Nil) => Try(s.reverse).map(v => StringValue(v))
    case _ =>
      fail(FilterApplicationError(parent, this, input, args))
  }
}
