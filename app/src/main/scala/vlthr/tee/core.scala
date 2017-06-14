package vlthr.tee.core

case class ParseContext(begin: Int, end: Int, template: Template) {
  
}
class EvalContext {
  def mappings: Map[String, Value] = ???
  def parentScope: EvalContext = ???
  def lookup(s: String): Value = ???
}
abstract class Expr {
  def eval()(evalContext: EvalContext): Value = ???
}
final case class AndExpr(left: Expr, right: Expr)(implicit context: ParseContext) extends Expr
final case class OrExpr(left: Expr, right: Expr)(implicit context: ParseContext) extends Expr
final case class EqExpr(left: Expr, right: Expr)(implicit context: ParseContext) extends Expr
final case class NEqExpr(left: Expr, right: Expr)(implicit context: ParseContext) extends Expr
final case class LEqExpr(left: Expr, right: Expr)(implicit context: ParseContext) extends Expr
final case class LtExpr(left: Expr, right: Expr)(implicit context: ParseContext) extends Expr
final case class GEqExpr(left: Expr, right: Expr)(implicit context: ParseContext) extends Expr
final case class GtExpr(left: Expr, right: Expr)(implicit context: ParseContext) extends Expr
final case class LiteralExpr(value: Value)(implicit context: ParseContext) extends Expr
final case class VariableUseExpr(name: String)(implicit context: ParseContext) extends Expr
final case class IndexExpr(indexable: Expr, key: Expr)(implicit context: ParseContext) extends Expr // l[0], l['hello'],
final case class DotExpr(indexable: Expr, key: Value)(implicit context: ParseContext) extends Expr // l.hello, or l.size (special methods)

sealed trait Node {
  def render()(implicit evalContext: EvalContext): String = ???
}
final case class TagNode(tag: Tag, args: List[Expr]) extends Node    // {% xxx %}
final case class OutputNode(expr: Expr) extends Node // {{ 'hello' }}

object Tag {
  def build(string: String): Tag = ???
  def register(tagName: String, ctor: Function1[String, Tag]): Unit = ???
}
abstract class Tag {
  def render(args: List[Expr])(implicit evalContext: EvalContext) = ???
}
final class AssignTag extends Tag
final class CaptureTag extends Tag
final class CaseTag extends Tag
final class CommentTag extends Tag
final class CycleTag extends Tag
final class ForTag extends Tag
final class BreakTag extends Tag
final class ContinueTag extends Tag
final class IfTag extends Tag
final class IncludeTag extends Tag
final class RawTag extends Tag
final class UnlessTag extends Tag

sealed trait Value {
  def render: String = ???
}
abstract class IndexedValue extends Value
final case class StringValue(v: String) extends Value
final case class BoolValue(v: Boolean) extends Value
final case class IntValue(v: Int) extends Value
final case class MapValue(v: Map[String, Value]) extends IndexedValue
final case class ListValue(v: List[Value]) extends IndexedValue

class Template(body: String, nodes: List[Node]) {
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
