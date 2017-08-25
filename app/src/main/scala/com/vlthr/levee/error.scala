package com.vlthr.levee.core
import org.antlr.v4.runtime.{Recognizer, RecognitionException}
import scala.util.{Try, Success, Failure}
import com.vlthr.levee.util.Util
import validation._
import validation.Result.{Valid, Invalids, Invalid}
import com.vlthr.levee.filters.Filter

package object error {
  type Validated[A] = Result[Error, A]
  type ValidatedFragment[A] = Result[ErrorFragment, A]

  /** Base type for errors with an associated location in the source template */
  trait Error(pctx: ParseContext) extends Throwable {
    def errorType: String

    def description: String

    override def getMessage: String = {
      s"""$errorType: ${pctx.sourcePosition.template.path}
   |$description
   |
   |    ${pctx.sourcePosition.report}""".stripMargin
    }

    override def toString: String = getMessage
  }

  /** An error which has no associated source location */
  trait ErrorFragment {
    def errorType: String

    def description: String

    /** Add a parse context to this fragment, forming an Error */
    def imbue(pctx: ParseContext): Error = ImbuedContext(this)(pctx)
  }

  /** A wrapper around an ErrorFragment that provides it with a parse context */
  case class ImbuedContext(parent: ErrorFragment)(pctx: ParseContext) extends Error(pctx) {
    def errorType = parent.errorType

    def description = parent.description
  }

  /** Returns the value as a Validated, i.e either a valid value or else an
    * invalid value containing Errors (with associated source info)
    *
    * For use in places where a ParseContext is available.
    */
  def valid[T](value: T): Validated[T] = Result.valid(value)

  def invalid[T](error: Error): Validated[T] = {
    Result.invalid(error)
  }

  /** Returns the value as a ValidatedFragment, i.e either a valid value or
    * else an invalid value containing ErrorFragments (with no source info).
    *
    * For use in places where no ParseContext is available, such as Filters.
    */
  def success[T](value: T): ValidatedFragment[T] = Result.valid(value)

  def failure[T](error: ErrorFragment): ValidatedFragment[T] = {
    Result.invalid(error)
  }


  /** Throws an exception, aborting execution of user code.
    *
    * For use in extensions when invalid state is detected and the user should
    * be alerted.
    */
  def abort[T](): ValidatedFragment[T] = throw new Exception("Extension execution aborted.")

  /** Converts ValidatedFragment[T] to a Validated[T] by imbuing it with source metadata */
  def imbueFragments[T](v: ValidatedFragment[T])(implicit pctx: ParseContext): Validated[T] = v match {
    case v @ Valid(_) => v
    case Invalids(errFragments) => Invalids(errFragments.map(_.imbue(pctx)))
    case Invalid(errFragment) => Invalid(errFragment.imbue(pctx))
  }

  /** Converts Validated[T] to a Try[T] */
  def toTry[T](v: Validated[T]) = v match {
    case Valid(output) => Success(output)
    case Invalids(errs) => Failure(LiquidFailure(errs.toList))
    case Invalid(errs) => Failure(LiquidFailure(errs :: Nil))
  }

  /** Flattens nested ValidatedFragment successes */
  def flatten[T, R <: ValidatedFragment[T]](v: ValidatedFragment[R]): ValidatedFragment[T] = v match {
    case Valid(output) => output
    case Invalids(errs) => Invalids(errs)
    case Invalid(errs) => Invalid(errs)
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
    def description = e.getMessage + "\n" + Util.getStackTrace(e)
  }

  case class InvalidKwArg(ext: Extension, key: String) extends ExtensionError {
    def description = s"${ext.extensionType} `${ext.name}` does not take a keyword argument named `$key`."
  }
  case class InvalidKwArgType(ext: Extension, key: String, value: Value, expectedType: ValueType) extends ExtensionError {
    def description = s"${ext.extensionType} `${ext.name}` keyword argument `$key` received expression of type ${value.valueType}. Required $expectedType."
  }
  case class InvalidTagArgs(tag: Tag, args: List[Value]) extends ExtensionError {
    override def errorType = "Parse Error"
    def description = s"Tag `${tag.name}` is not defined for arguments (${args.map(_.valueType).mkString(", ")})."
  }
  case class UnknownTagId(id: String) extends ExtensionError {
    override def errorType = "Parse Error"
    def description = s"`$id` does not match any known tag."
  }
  case class UnexpectedValueType(v: Value, expected: Option[ValueType] = None) extends ExtensionError  {
    override def errorType = "Type Error"
    def description = expected match {
      case Some(e) => s"Unexpected value type: ${v.valueType}. Expected $e."
      case None => s"Unexpected value type: ${v.valueType}."
    }
  }

  case class MalformedTag()(implicit pctx: ParseContext) extends Error(pctx) {
    def errorType = "Parse Error"
    def description = s"Malformed tag."
  }

  case class UnrenderableValue(value: Value) extends ErrorFragment {
    def errorType = "Render Error"
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
  trait ExtensionError extends ErrorFragment {
    def errorType = "Extension Error"
  }
  case class InvalidInput(ext: Extension, input: Value) extends ExtensionError {
    def description = s"${ext.extensionType} `${ext.name}` is not defined for input type ${input.valueType}."
  }
  case class InvalidArgs(ext: Extension, args: List[Value]) extends ExtensionError {
    def description = s"${ext.extensionType} `${ext.name}` is not defined for arguments (${args.map(_.valueType).mkString(", ")})."
  }
  case class InvalidOptArgs(ext: Extension, args: List[Value]) extends ExtensionError {
    def description = s"${ext.extensionType} `${ext.name}` is not defined for optional arguments (${args.map(_.valueType).mkString(", ")})."
  }
  case class TooManyArgs(ext: Extension, args: List[Value]) extends ExtensionError {
    def description = s"Too many arguments to ${ext.extensionType} ${ext.name}."
  }
  case class TooFewArgs(ext: Extension, args: List[Value]) extends ExtensionError {
    def description = s"Too many arguments to ${ext.extensionType} ${ext.name}."
  }
  case class UnknownFilterName(name: String) extends ExtensionError {
    def description = s"Unknown filter: `$name`."
  }
  case class FilterApplicationError(expr: FilterExpr, filter: Filter, input: Value, args: List[Value]) extends RenderError(expr.pctx) {
    def description = s"Filter `${filter.name}` is not defined for input type ${input.valueType} and arguments (${args.map(_.valueType).mkString(", ")})."
  }
  case class LiquidFailure(errors: List[Error]) extends Exception {
    override def getMessage(): String = errors.mkString("\n")
    override def toString = getMessage
  }
}
