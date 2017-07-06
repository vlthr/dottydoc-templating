package vlthr.tee.core
import scala.util.{Try, Success, Failure}

trait Error(sourcePosition: SourcePosition) {
  def getMessage: String = {
    s"""
   |$errorType: ${sourcePosition.template.path}
   |$description
   |${sourcePosition.report}
   """.stripMargin
  }
  def errorType: String
  def description: String
  override def toString: String = getMessage
}
abstract class TypeError(sourcePosition: SourcePosition) extends Error(sourcePosition) {
  def errorType = "Type Error"
}
abstract class RenderError(sourcePosition: SourcePosition) extends Error(sourcePosition) {
  def errorType = "Render Error"
}
abstract class RuntimeError(sourcePosition: SourcePosition) extends Error(sourcePosition) {
  def errorType = "Unexpected Runtime Error"
}

case class ParseError(override val description: String)(implicit sourcePosition: SourcePosition) extends Error(sourcePosition) {
  def errorType = "Parse Error"
}

case class InvalidIterable(expr: Expr) extends TypeError(expr.sourcePosition) {
  def description = s"Expected an iterable"
}
case class InvalidMap(expr: DotExpr, map: Expr) extends TypeError(map.sourcePosition) {
  def description = s"Type ${map} can not be indexed as a map."
}
case class UndefinedVariable(expr: Expr) extends RenderError(expr.sourcePosition) {
  def description = s"Undefined variable reference"
}
case class UndefinedField(expr: DotExpr, map: Expr, field: String) extends RenderError(expr.sourcePosition) {
  def description = s"Map $expr contains no field `$field`"
}
case class InvalidIndex(expr: IndexExpr, index: Expr) extends TypeError(index.sourcePosition) {
  def description = s"Invalid index type: $index"
}
case class InvalidIndexable(expr: IndexExpr, indexable: Expr) extends TypeError(expr.sourcePosition) {
  def description = s"$indexable is not an indexable"
}
case class IncomparableValues(expr: Expr, left: Value, right: Value) extends TypeError(expr.sourcePosition) {
  def description = s"Incomparable values $left and $right"
}
case class InvalidInclude(obj: IncludeTag, filename: Value) extends TypeError(obj.sourcePosition) {
  def description = s"Include tag argument must be a filename, not $filename"
}
case class InvalidFilterInput(obj: FilterExpr, filter: Filter, input: Value) extends TypeError(obj.sourcePosition) {
  def description = s"Filter ${filter.name} is not defined for input $input."
}
case class InvalidFilterArgs(obj: FilterExpr, filter: Filter, args: List[Value]) extends TypeError(obj.sourcePosition) {
  def description = s"Filter ${filter.name} is not defined for arguments $args."
}
case class FilterApplicationError(obj: FilterExpr, filter: Filter, input: Value, args: List[Value]) extends RenderError(obj.sourcePosition) {
  def description = s"Filter ${filter.name} is not defined for input $input and arguments $args."
}
case class LiquidFailure(errors: List[Error]) extends Exception {
  override def getMessage(): String = errors.mkString("\n")
  override def toString = getMessage
}

object Error {
  def all[A, B](a: Try[A])(onSuccess: Function[A, B]): Try[B] =
    all(Success(null), a)((_, r) => onSuccess(r))

  // TODO: can this be made to work for any number of args?
  def all[A, B, C](a: Try[A], b: Try[B])(
      onSuccess: Function[(A, B), C]): Try[C] = {
    (a, b) match {
      case (Success(innerA), Success(innerB)) =>
        Success(onSuccess(innerA, innerB))
      case (a, b) => Failure(LiquidFailure(extractErrors(a, b)))
    }
  }

  def all[A, B](as: Try[A]*)(onSuccess: Function[A, B]): Try[List[B]] = {
    val (successes, failures) = as.partition(_.isSuccess)
    if (failures.size > 0) fail(extractErrors(failures: _*): _*)
    else Success(successes.map(s => onSuccess(s.get)).toList)
  }

  def condenseAll[A, B](as: Try[A]*)(
      onSuccess: Function[List[A], Try[B]]): Try[B] = {
    val (successes, failures) = as.partition(_.isSuccess)
    if (failures.size > 0) fail(extractErrors(failures: _*): _*)
    else onSuccess(successes.map(s => s.get).toList)
  }

  def extractErrors[T](sources: Try[T]*): List[Error] = {
    sources.flatMap {
      case Failure(LiquidFailure(errors)) => errors
      case _ => List()
    }.toList
  }

  def fail[T](e: Error*): Try[T] = Failure(LiquidFailure(e.toList))
}
