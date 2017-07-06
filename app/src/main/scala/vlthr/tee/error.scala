package vlthr.tee.core
import scala.util.{Try, Success, Failure}

sealed trait Error
final case class ParseError(msg: String) extends Error
final case class TypeError(msg: String) extends Error
final case class RenderError(msg: String) extends Error
final case class NameError(msg: String) extends Error

case class LiquidFailure(errors: List[Error]) extends Exception {
  override def getMessage(): String = errors.mkString("\n")
  override def toString = getMessage
}

object Error {
  def invalidIterable(iterableExpr: Expr) = {
    val msg = s"""Type Error: Invalid iterable
${iterableExpr.sourcePosition.report}
"""
    fail(TypeError(msg))
  }

  def invalidMap(dotExpr: DotExpr, map: Expr) = {
    val msg = s"""Type Error: Invalid map
${map.sourcePosition.report}
"""
    fail(TypeError(msg))
  }

  def undefinedVariable(idUse: Expr) = {
    val msg = s"""Error: Undefined variable reference
${idUse.sourcePosition.report}
"""
    fail(RenderError(msg))
  }

  def undefinedField(dotExpr: DotExpr, map: Expr, field: String) = {
    val msg = s"""Error: hashmap ${map.toString} contains no field '$field'
${dotExpr.sourcePosition.report}
"""
    fail(RenderError(msg))
  }

  def invalidIndex(indexExpr: IndexExpr, index: Expr) = {
    val msg = s"""Error: invalid index
${index.sourcePosition.report}
"""
    fail(TypeError(msg))
  }

  def invalidIndexable(indexExpr: IndexExpr, indexable: Expr) = {
    val msg = s"""Error: ${indexable.toString} is not indexable
${indexExpr.sourcePosition.report}
"""
    fail(TypeError(msg))
  }

  def incomparableValues(expr: Expr, left: Value, right: Value) = {
    val msg = s"""Error: Incomparable values $left and $right
${expr.sourcePosition.report}
"""
    fail(TypeError(msg))
  }

  def invalidInclude(include: IncludeTag, filename: Value) = {
    val msg = s"""Error: include tag argument must be a filename, not $filename
${include.sourcePosition.report}
"""
    fail(TypeError(msg))
  }

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
