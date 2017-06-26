package vlthr.tee.core
import scala.collection.mutable.Map

case class ParseContext(begin: Int, end: Int, template: Template) {}

case class EvalContext(mappings: Map[String, Value], parent: Option[EvalContext]) {
  def lookup(s: String): Option[Value] = mappings.get(s).orElse(parent.flatMap(_.lookup(s)))
}

object EvalContext {
  def createNew(): EvalContext = EvalContext(Map(), None)
  def createNew(map: Map[String, Value]): EvalContext = EvalContext(map, None)
  def createChild(parent: EvalContext): EvalContext = EvalContext(Map(), Some(parent))
}

abstract trait Node extends Renderable {
  def parseContext: ParseContext
}

trait Renderable {
  def render()(implicit evalContext: EvalContext): String
}

final case class BlockNode(node: List[Node])(
    implicit val parseContext: ParseContext)
    extends Node {
  def render()(implicit evalContext: EvalContext): String = {
    implicit val newScope = EvalContext.createChild(evalContext)
    node.map(_.render()).mkString
  }
}

final case class OutputNode(expr: Expr)(
    implicit val parseContext: ParseContext)
    extends Node {
  def render()(implicit evalContext: EvalContext): String = expr.render()
}

final case class TextNode(text: String)(
    implicit val parseContext: ParseContext)
    extends Node {
  def render()(implicit evalContext: EvalContext): String = text
}

trait TagNode extends Node with Renderable {
  def render()(implicit evalContext: EvalContext): String = ???
}

final case class AssignTag(id: String, value: Expr)(
    implicit val parseContext: ParseContext)
    extends TagNode {
  override def render()(implicit evalContext: EvalContext): String = {
    evalContext.mappings.put(id, value.eval())
    ""
  }
}
final case class CaptureTag()(implicit val parseContext: ParseContext)
    extends TagNode
final case class CaseTag()(implicit val parseContext: ParseContext)
    extends TagNode
final case class CommentTag()(implicit val parseContext: ParseContext)
    extends TagNode
final case class CycleTag()(implicit val parseContext: ParseContext)
    extends TagNode
final case class ForTag(id: String, expr: Expr, block: Node)(
    implicit val parseContext: ParseContext)
    extends TagNode {
  override def render()(implicit evalContext: EvalContext): String = {
    val iterable = expr.eval match {
      case ListValue(l) => l
      case _ => throw new Exception(s"TODO: $expr is not an iterable")
    }
    iterable.map { v =>
      implicit val forCtx = EvalContext.createChild(evalContext)
      forCtx.mappings.put(id, Value.create(v))
      block.render
    }.mkString
  }
}
final case class BreakTag()(implicit val parseContext: ParseContext)
    extends TagNode
final case class ContinueTag()(implicit val parseContext: ParseContext)
    extends TagNode
final case class IfTag(condition: Expr, block: Node)(
    implicit val parseContext: ParseContext)
    extends TagNode {
  override def render()(implicit evalContext: EvalContext): String = if (condition.eval.truthy) block.render() else ""
}
final case class IncludeTag()(implicit val parseContext: ParseContext)
    extends TagNode
final case class RawTag()(implicit val parseContext: ParseContext)
    extends TagNode
final case class UnlessTag()(implicit val parseContext: ParseContext)
    extends TagNode

abstract trait Expr extends Renderable {
  def parseContext: ParseContext
  def eval()(implicit evalContext: EvalContext): Value = ???
  def render()(implicit evalContext: EvalContext): String = eval().render
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
    implicit val parseContext: ParseContext)
    extends Expr
final case class OrExpr(left: Expr, right: Expr)(
    implicit val parseContext: ParseContext)
    extends Expr
final case class EqExpr(left: Expr, right: Expr)(
    implicit val parseContext: ParseContext)
    extends Expr
final case class NEqExpr(left: Expr, right: Expr)(
    implicit val parseContext: ParseContext)
    extends Expr
final case class LEqExpr(left: Expr, right: Expr)(
    implicit val parseContext: ParseContext)
    extends Expr
final case class LtExpr(left: Expr, right: Expr)(
    implicit val parseContext: ParseContext)
    extends Expr
final case class GEqExpr(left: Expr, right: Expr)(
    implicit val parseContext: ParseContext)
    extends Expr
final case class GtExpr(left: Expr, right: Expr)(
    implicit val parseContext: ParseContext)
    extends Expr
final case class LiteralExpr(value: Value)(
    implicit val parseContext: ParseContext)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = value
}
final case class VariableUseExpr(name: String)(
    implicit val parseContext: ParseContext)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = {
    evalContext.lookup(name) match {
      case Some(value) => value
      case None => throw new Exception(s"TODO: variable '$name' not found")
    }
  }
}
final case class FilterExpr(expr: Expr, filter: Filter, args: List[Expr])(
    implicit val parseContext: ParseContext)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = filter.apply(expr.eval())
}
final case class IndexExpr(indexable: Expr, key: Expr)(
    implicit val parseContext: ParseContext)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = {
    val source = indexable.eval()
    val index = key.eval() match {
      case IntValue(i) => i
      case _ => throw new Exception("TODO: Non int index")
    }
    source match {
      case ListValue(s) => s(index)
      case _ => throw new Exception("TODO: Invalid indexable")
    }
  }
}
final case class DotExpr(indexable: Expr, key: String)(
    implicit val parseContext: ParseContext)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = {
    val source = indexable.eval()
    source match {
      case MapValue(s) => s.get(key).get
      case _ => throw new Exception("TODO: Invalid indexable")
    }
  }
}

sealed trait Value extends Renderable with Truthable {
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
  def render()(implicit evalContext: EvalContext): String = v
}
final case class BooleanValue(v: Boolean) extends Value {
  def render()(implicit evalContext: EvalContext): String = v.toString
  def truthy = v
}
final case class IntValue(v: Int) extends Value with Truthy {
  def render()(implicit evalContext: EvalContext): String = v.toString
}
final case class MapValue(v: Map[String, Value]) extends IndexedValue with Truthy {
  def render()(implicit evalContext: EvalContext): String = ???
}
final case class ListValue(v: List[Value]) extends IndexedValue with Truthy {
  def render()(implicit evalContext: EvalContext): String = ???
}

case class Template(body: String, nodes: List[Node]) {
  def this(path: String) = {
    // read file, populate body/nodes
    this("", Nil)
  }
  def render(evalContext: EvalContext): String = {
    // eval nodes
    // replace nodes with evaled values in body
    body
  }

}
