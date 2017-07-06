package vlthr.tee.filters
import scala.util.{Try, Success, Failure}
import vlthr.tee.parser.Liquid
import vlthr.tee.core._
import vlthr.tee.core.Error._

object Filter {
  def byName(s: String): Filter = registry.get(s).getOrElse(NoFilter())
  val registry: Map[String, Filter] = Map("split" -> Split(), "size" -> Size())
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

case class Size() extends Filter {
  def name = "size"
  def isDefinedForInput(v: Value): Boolean = v match {
    case ListValue(_) => true
    case _ => false
  }
  def isDefinedForArgs(v: List[Value]): Boolean = v match {
    case Nil => true
    case _ => false
  }
  def apply(input: Value, args: List[Value])(
      implicit evalContext: EvalContext, parent: FilterExpr) = (input, args) match {
    case (ListValue(l), Nil) => Try(IntValue(l.size))
    case _ =>
      fail(FilterApplicationError(parent, this, input, args))
  }
}
