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

case class UncaughtExceptionError(e: Throwable)(implicit pctx: ParseContext) extends Error(pctx) {
  def errorType = "Uncaught Exception"
  def description = e.getMessage
}

case class TagException(desc: String) extends Exception(desc)
case class TagError(desc: String)(implicit pctx: ParseContext) extends Error(pctx) {
  def errorType = "Custom Tag Error"
  def description = desc
}

case class InvalidTagArgs(tagNode: CustomTag, tag: Tag, args: List[Value]) extends Error(tagNode.pctx) {
  def errorType = "Parse Error"
  def description = s"Tag `${tag.name}` is not defined for arguments (${args.map(_.valueType).mkString(", ")})."
}
case class UnknownTagIdException(id: String) extends Throwable
case class UnknownTagId(id: String)(implicit pctx: ParseContext) extends Error(pctx) {
  def errorType = "Parse Error"
  def description = s"`$id` does not match any known tag."
}
case class UnexpectedValueType(expr: Expr, v: Value) extends TypeError(expr.pctx) {
  def description = s"Unexpected value type: ${v.valueType}"
}

case class MalformedTagException(error: MalformedTag) extends Throwable
case class MalformedTag()(implicit pctx: ParseContext) extends Error(pctx) {
  def errorType = "Parse Error"
  def description = s"Malformed tag."
}

case class UnrenderableValueException() extends Throwable
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
case class FilterException(desc: String) extends Throwable
case class FilterError(desc: String)(implicit pctx: ParseContext) extends Error(pctx) {
  def errorType = "Filter Error"
  def description = desc
}
case class InvalidFilterInput(expr: FilterExpr, filter: Filter, input: Value) extends TypeError(expr.pctx) {
  def description = s"Filter `${filter.name}` is not defined for input type ${input.valueType}."
}
case class InvalidFilterArgs(expr: FilterExpr, filter: Filter, args: List[Value]) extends TypeError(expr.pctx) {
  def description = s"Filter `${filter.name}` is not defined for arguments (${args.map(_.valueType).mkString(", ")})."
}
case class TooManyFilterArgs(expr: FilterExpr, filter: Filter, args: List[Value]) extends TypeError(expr.pctx) {
  def description = s"Too many arguments to filter ${filter.name}."
}
case class UnknownFilterNameException(name: String) extends Exception
case class UnknownFilterName(name: String)(implicit pctx: ParseContext) extends RenderError(pctx) {
  def description = s"Unknown filter: `$name`."
}
case class FilterApplicationError(expr: FilterExpr, filter: Filter, input: Value, args: List[Value]) extends RenderError(expr.pctx) {
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
      case Success(_) => List()
      case _ => throw new Exception(s"Liquid Error: Unexpected argument to Error.extractErrors: $sources")
    }.toList
  }

  def fail[T](e: Error*): Try[T] = Failure(LiquidFailure(e.toList))
}
