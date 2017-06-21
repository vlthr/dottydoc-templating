package vlthr.tee.core

case class ParseContext(begin: Int, end: Int, template: Template) {}

trait EvalContext {
  def mappings: Map[String, Value] = ???
  def parentScope: EvalContext = ???
  def lookup(s: String): Value = ???
}

abstract trait Node {
  def render()(implicit evalContext: EvalContext): String = ???
  def parseContext: ParseContext
}

final case class BlockNode(node: List[Node])(
    implicit val parseContext: ParseContext)
    extends Node {}

final case class OutputNode(expr: Expr)(
    implicit val parseContext: ParseContext)
    extends Node // {{ 'hello' }}

final case class TextNode(text: String)(
    implicit val parseContext: ParseContext)
    extends Node

trait TagNode extends Node {
  def render(args: List[Expr])(implicit evalContext: EvalContext) = ???
}

final case class AssignTag(id: String, value: Expr)(
    implicit val parseContext: ParseContext)
    extends TagNode
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
    extends TagNode
final case class BreakTag()(implicit val parseContext: ParseContext)
    extends TagNode
final case class ContinueTag()(implicit val parseContext: ParseContext)
    extends TagNode
final case class IfTag(condition: Expr, block: Node)(
    implicit val parseContext: ParseContext)
    extends TagNode
final case class IncludeTag()(implicit val parseContext: ParseContext)
    extends TagNode
final case class RawTag()(implicit val parseContext: ParseContext)
    extends TagNode
final case class UnlessTag()(implicit val parseContext: ParseContext)
    extends TagNode

abstract trait Expr {
  def parseContext: ParseContext
  def eval()(evalContext: EvalContext): Value = ???
}

trait Filter {
  def apply(args: List[Expr]): Value = ???
}

object Filter {
  def byName(s: String): Filter = new Object() with Filter
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
    extends Expr
final case class VariableUseExpr(name: String)(
    implicit val parseContext: ParseContext)
    extends Expr
final case class FilterExpr(expr: Expr, filter: Filter, args: List[Expr])(
    implicit val parseContext: ParseContext)
    extends Expr
final case class IndexExpr(indexable: Expr, key: Expr)(
    implicit val parseContext: ParseContext)
    extends Expr // l[0], l['hello'],
final case class DotExpr(indexable: Expr, key: String)(
    implicit val parseContext: ParseContext)
    extends Expr // l.hello, or l.size (special methods)

sealed trait Value {
  def render: String = ???
}

sealed trait IndexedValue extends Value
final case class StringValue(v: String) extends Value
final case class BooleanValue(v: Boolean) extends Value
final case class IntValue(v: Int) extends Value
final case class MapValue(v: Map[String, Value]) extends IndexedValue
final case class ListValue(v: List[Value]) extends IndexedValue

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
