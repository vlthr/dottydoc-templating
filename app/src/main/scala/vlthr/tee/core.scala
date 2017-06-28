package vlthr.tee.core
import scala.collection.mutable.Map
import scala.util.{Try, Success, Failure}

sealed trait Error

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

  def all[A, B](a: Try[A])(onSuccess: Function[A, B]): Try[B] = all(Success(null), a)((_, r) => onSuccess(r))

  // TODO: can this be made to work for any number of args?
  def all[A, B, C](a: Try[A], b: Try[B])(onSuccess: Function[(A, B), C]): Try[C] = {
    (a, b) match {
      case (Success(innerA), Success(innerB)) => Success(onSuccess(innerA, innerB))
      case (a, b) => Failure(LiquidFailure(extractErrors(a, b)))
    }
  }

  def all[A, B](as: Try[A]*)(onSuccess: Function[A, B]): Try[List[B]] = {
    val (successes, failures) = as.partition(_.isSuccess)
    if (failures.size > 0) fail(extractErrors(failures: _*): _*)
    else Success(successes.map(s => onSuccess(s.get)).toList)
  }

  def extractErrors[T](sources: Try[T]*): List[Error] = {
    sources.flatMap {
      case Failure(LiquidFailure(errors)) => errors
      case _ => List()
    }.toList
  }

  def fail[T](e: Error*): Try[T] = Failure(LiquidFailure(e.toList))
}

final case class ParseError(msg: String) extends Error
final case class TypeError(msg: String) extends Error
final case class RenderError(msg: String) extends Error
final case class NameError(msg: String) extends Error

case class LiquidFailure(errors: List[Error]) extends Exception(errors.mkString("\n")) {
  override def getMessage(): String = errors.toString
  override def toString = getMessage
}

case class SourcePosition(start: Int, end: Int, template: SourceFile) {
  def report: String = template.body.substring(start, end+1)
}

case class EvalContext(mappings: Map[String, Value],
                       parent: Option[EvalContext]) {
  def lookup(s: String): Option[Value] =
    mappings.get(s).orElse(parent.flatMap(_.lookup(s)))
}

object EvalContext {
  def createNew(): EvalContext = EvalContext(Map(), None)
  def createNew(map: Map[String, Value]): EvalContext = EvalContext(map, None)
  def createChild(parent: EvalContext): EvalContext =
    EvalContext(Map(), Some(parent))
}

abstract trait Node extends Renderable {
  def sourcePosition: SourcePosition
}

trait Renderable {
  def render()(implicit evalContext: EvalContext): Try[String]
}

final case class BlockNode(node: List[Node])(
    implicit val sourcePosition: SourcePosition)
    extends Node {
  def render()(implicit evalContext: EvalContext) = {
    implicit val newScope = EvalContext.createChild(evalContext)
    val renders = node.map(_.render())
    Error.all(renders: _*)(r => r).map(_.mkString)
  }
}

final case class OutputNode(expr: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends Node {
  def render()(implicit evalContext: EvalContext) = Error.all(expr.render) { e =>
    e
  }
}

final case class TextNode(text: String)(
    implicit val sourcePosition: SourcePosition)
    extends Node {
  def render()(implicit evalContext: EvalContext): Try[String] = Success(text)
}

trait TagNode extends Node with Renderable {
  def render()(implicit evalContext: EvalContext): Try[String] = ???
}

final case class AssignTag(id: String, value: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends TagNode {
  override def render()(implicit evalContext: EvalContext): Try[String] = {
    Error.all(value.eval) { v: Value =>
      evalContext.mappings.put(id, v)
      ""
    }
  }
}
final case class CaptureTag()(implicit val sourcePosition: SourcePosition)
    extends TagNode
final case class CaseTag()(implicit val sourcePosition: SourcePosition)
    extends TagNode
final case class CommentTag()(implicit val sourcePosition: SourcePosition)
    extends TagNode
final case class CycleTag()(implicit val sourcePosition: SourcePosition)
    extends TagNode
final case class ForTag(id: String, expr: Expr, block: Node)(
  implicit val sourcePosition: SourcePosition)
    extends TagNode {
  override def render()(implicit evalContext: EvalContext): Try[String] = {
    val iterable: Try[List[_]] = expr.eval.flatMap {
      case ListValue(l) => Success(l)
      case _ => Error.invalidIterable(expr)
    }
    println(iterable)
    Error.all(iterable) { iterable =>
      val renders = iterable.map { v =>
        implicit val forCtx = EvalContext.createChild(evalContext)
        forCtx.mappings.put(id, Value.create(v))
        block.render
      }
      Error.all(renders: _*)(r => r).map(_.mkString)
    }.flatten
  }
}
final case class BreakTag()(implicit val sourcePosition: SourcePosition)
    extends TagNode
final case class ContinueTag()(implicit val sourcePosition: SourcePosition)
    extends TagNode
final case class IfTag(condition: Expr, block: Node)(
    implicit val sourcePosition: SourcePosition)
    extends TagNode {
  override def render()(implicit evalContext: EvalContext): Try[String] = {
    val cond = condition.eval
    val render = block.render

    Error.all(condition.eval, block.render) { (cond, render) =>
      if (cond.truthy) render
      else ""
    }
  }
}
final case class IncludeTag()(implicit val sourcePosition: SourcePosition)
    extends TagNode
final case class RawTag()(implicit val sourcePosition: SourcePosition)
    extends TagNode
final case class UnlessTag()(implicit val sourcePosition: SourcePosition)
    extends TagNode

abstract trait Expr extends Renderable {
  def sourcePosition: SourcePosition
  def eval()(implicit evalContext: EvalContext): Try[Value] = ???
  def render()(implicit evalContext: EvalContext): Try[String] = eval.flatMap(_.render)
}

abstract trait Filter {
  def apply(input: Value): Value
}

case class NoFilter() extends Filter {
  def apply(input: Value) = input
}

object Filter {
  def byName(s: String): Filter = NoFilter()
}

final case class AndExpr(left: Expr, right: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = Error.all(left.eval, right.eval) { (l, r) =>
    BooleanValue(l.truthy && r.truthy)
  }
}
final case class OrExpr(left: Expr, right: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = Error.all(left.eval, right.eval) { (l, r) =>
    BooleanValue(l.truthy || r.truthy)
  }
}
final case class EqExpr(left: Expr, right: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = Error.all(left.eval, right.eval) { (l, r) =>
    BooleanValue(l == r)
  }
}
final case class NEqExpr(left: Expr, right: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = Error.all(left.eval, right.eval) { (l, r) =>
    BooleanValue(l != r)
  }
}
final case class LEqExpr(left: Expr, right: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = Error.all(left.eval, right.eval) { (l, r) =>
    BooleanValue(l <= r)
  }
}
final case class LtExpr(left: Expr, right: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = Error.all(left.eval, right.eval) { (l, r) =>
    BooleanValue(l < r)
  }
}
final case class GEqExpr(left: Expr, right: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = Error.all(left.eval, right.eval) { (l, r) =>
    BooleanValue(l >= r)
  }
}
final case class GtExpr(left: Expr, right: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = Error.all(left.eval, right.eval) { (l, r) =>
    BooleanValue(l > r)
  }
}
final case class LiteralExpr(value: Value)(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = Success(value)
}
final case class VariableUseExpr(name: String)(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = {
    evalContext.lookup(name) match {
      case Some(value) => Success(value)
      case None => Error.undefinedVariable(this)
    }
  }
}
final case class FilterExpr(expr: Expr, filter: Filter, args: List[Expr])(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) =
    Error.all(expr.eval){ result => filter.apply(result) }
}
final case class IndexExpr(indexable: Expr, key: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = {
    val i: Try[List[Value]] = indexable.eval.flatMap {
      case ListValue(s) => Success(s)
      case x => Error.invalidIndexable(this, indexable)
    }
    val k: Try[Int] = key.eval.flatMap {
      case IntValue(i) => Success(i)
      case x => Error.invalidIndex(this, key)
    }
    Error.all(i, k) { (i, k) =>
      i(k)
    }

  }
}
final case class DotExpr(indexable: Expr, key: String)(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = {
    val source: Try[Map[String, Value]] = indexable.eval.flatMap {
      case MapValue(m) => Success(m)
      case x => Error.invalidMap(this, indexable)
    }
    Error.all(source) { source =>
      source.get(key) match {
        case Some(s) => Success(s)
        case None => Error.undefinedField(this, indexable, key)
      }
    }.flatten
  }
}

sealed trait Value extends Renderable with Truthable with Ordered[Value] {
  def compare(that: Value): Int = {
    (this, that) match {
      case (IntValue(l), IntValue(r)) => l compare r
      case (StringValue(l), StringValue(r)) => l compare r
      case (BooleanValue(l), BooleanValue(r)) => l compare r
      case (MapValue(l), MapValue(r)) => ???
      case (ListValue(l), ListValue(r)) => ???
      case (l, r) => throw new Exception(s"TODO: Incomparable types $l and $r")
    }
  }
}

object Value {
  def create(value: Any) = {
    value match {
      case v: Value => v
      case v: Int => IntValue(v)
      case v: String => StringValue(v)
      case v: Boolean => BooleanValue(v)
      case v: Map[String, Value] => MapValue(v)
      case v: List[Value] => ListValue(v)
      case _ => throw new Exception(s"Invalid value: $value")
    }
  }
}

sealed trait Truthable {
  def truthy: Boolean
}
trait Truthy extends Truthable {
  def truthy = true
}
sealed trait IndexedValue extends Value
final case class StringValue(v: String) extends Value with Truthy {
  def render()(implicit evalContext: EvalContext): Try[String] = Success(v)
}
final case class BooleanValue(v: Boolean) extends Value {
  def render()(implicit evalContext: EvalContext): Try[String] = Success(v.toString)
  def truthy = v
}
final case class IntValue(v: Int) extends Value with Truthy {
  def render()(implicit evalContext: EvalContext): Try[String] = Success(v.toString)
}
final case class MapValue(v: Map[String, Value])
    extends IndexedValue
    with Truthy {
  def render()(implicit evalContext: EvalContext): Try[String] = ???
}
final case class ListValue(v: List[Value]) extends IndexedValue with Truthy {
  def render()(implicit evalContext: EvalContext): Try[String] = ???
}

case class SourceFile(body: String) {
  def render(evalContext: EvalContext): String = {
    // eval nodes
    // replace nodes with evaled values in body
    body
  }
}
