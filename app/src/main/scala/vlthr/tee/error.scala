package vlthr.tee.core
import org.antlr.v4.runtime.{Recognizer, RecognitionException}
import scala.util.{Try, Success, Failure}

trait Error(pctx: ParseContext) {
  def getMessage: String = {
    s"""$errorType: ${pctx.sourcePosition.template.path}
   |$description
   |
   |    ${pctx.sourcePosition.report}""".stripMargin
  }
  def errorType: String
  def description: String
  override def toString: String = getMessage
}
abstract class TypeError(pctx: ParseContext) extends Error(pctx) {
  def errorType = "Type Error"
}
abstract class RenderError(pctx: ParseContext) extends Error(pctx) {
  def errorType = "Render Error"
}
abstract class RuntimeError(pctx: ParseContext) extends Error(pctx) {
  def errorType = "Unexpected Runtime Error"
}

case class ParseError(recognizer: Recognizer[_, _], offendingSymbol: Object, override val description: String, e: RecognitionException)(implicit pctx: ParseContext) extends Error(pctx) {
  def errorType = "Parse Error"
}

case class InvalidTagIdException(error: InvalidTagId) extends Exception
case class InvalidTagId(id: String)(implicit pctx: ParseContext) extends Error(pctx) {
  def errorType = "Parse Error"
  def description = s"`$id` does not match any known tag."
}
case class UnexpectedValueType(v: Value)(implicit pctx: ParseContext) extends TypeError(pctx) {
  def description = s"Unexpected value type: ${v.valueType}"
}

case class MalformedTagException(error: MalformedTag) extends Exception
case class MalformedTag()(implicit pctx: ParseContext) extends Error(pctx) {
  def errorType = "Parse Error"
  def description = s"Malformed tag."
}

case class UnrenderableValueException() extends Exception
case class UnrenderableValue(expr: Expr, value: Value) extends RenderError(expr.pctx) {
  def description = s"Cannot render type ${value.valueType}"
}
case class InvalidIterable(expr: Expr) extends TypeError(expr.pctx) {
  def description = s"Expected an iterable"
}
case class InvalidMap(expr: DotExpr, map: Expr, value: Value) extends TypeError(map.pctx) {
  def description = s"Expression `${map.pctx.sourcePosition.display}` of type ${value.valueType} can not be indexed as a map."
}
case class UndefinedVariable(expr: VariableUseExpr) extends RenderError(expr.pctx) {
  def description = s"Undefined variable reference `${expr.pctx.sourcePosition.display}`"
}
case class UndefinedField(expr: DotExpr, map: Expr, field: String) extends RenderError(expr.pctx) {
  def description = s"Map `${map.sourcePosition.display}` contains no field `$field`"
}
case class InvalidIndex(expr: IndexExpr, index: Expr, value: Value) extends TypeError(index.pctx) {
  def description = s"Invalid index type: ${value.valueType}"
}
case class InvalidIndexable(expr: IndexExpr, indexable: Expr, value: Value) extends TypeError(expr.pctx) {
  def description = s"Expression `${indexable.sourcePosition.display}` of type `${value.valueType}` can not be indexed."
}
case class IncomparableValues(expr: Expr, left: Value, right: Value) extends TypeError(expr.pctx) {
  def description = s"Incomparable values ${left.valueType} and ${right.valueType}"
}
case class InvalidInclude(obj: IncludeTag, filename: Value) extends TypeError(obj.pctx) {
  def description = s"Include tag argument must be a filename, not ${filename.valueType}"
}
case class InvalidFilterInput(filter: Filter, input: Value) extends TypeError(filter.pctx) {
  def description = s"Filter `${filter.name}` is not defined for input type ${input.valueType}."
}
case class InvalidFilterArgs(filter: Filter, args: List[Value]) extends TypeError(filter.pctx) {
  def description = s"Filter `${filter.name}` is not defined for arguments (${args.map(_.valueType).mkString(", ")})."
}
case class FilterApplicationError(obj: FilterExpr, filter: Filter, input: Value, args: List[Value]) extends RenderError(obj.pctx) {
  def description = s"Filter `${filter.name}` is not defined for input type ${input.valueType} and arguments (${args.map(_.valueType).mkString(", ")})."
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
