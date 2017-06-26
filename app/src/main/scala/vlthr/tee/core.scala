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
  def codeGen(): String = ???
}

final case class BlockNode(node: List[Node])(
    implicit val parseContext: ParseContext)
    extends Node {
  override def codeGen(): String = node.map(_.codeGen()).mkString(" +\n")
}

final case class OutputNode(expr: Expr)(
    implicit val parseContext: ParseContext)
    extends Node {
  override def codeGen(): String = s"""{ ${expr.codeGen()} }.toString"""
}

final case class TextNode(text: String)(
    implicit val parseContext: ParseContext)
    extends Node {
  override def codeGen(): String = CodeGenUtil.escape(s"""'$text'""")
}

trait TagNode extends Node {
  def render(args: List[Expr])(implicit evalContext: EvalContext) = ???
  override def codeGen(): String = ""
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
  def eval(): Value = ???
  def codeGen(): String = ???
}

abstract trait Filter {
  def apply(expr: Expr, args: List[Expr]): Value
}

case class NoFilter() extends Filter {
  def apply(expr: Expr, args: List[Expr]): Value = expr.eval()
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
  override def codeGen(): String = value.codeGen()
}
final case class VariableUseExpr(name: String)(
    implicit val parseContext: ParseContext)
    extends Expr {
  override def codeGen(): String = name
}
final case class FilterExpr(expr: Expr, filter: Filter, args: List[Expr])(
    implicit val parseContext: ParseContext)
    extends Expr {
  override def codeGen(): String = expr.codeGen()
}
final case class IndexExpr(indexable: Expr, key: Expr)(
    implicit val parseContext: ParseContext)
    extends Expr {
  override def codeGen(): String = s"${indexable.codeGen()}(${key.codeGen()})"
}
final case class DotExpr(indexable: Expr, key: String)(
    implicit val parseContext: ParseContext)
    extends Expr {
  override def codeGen(): String = s"${indexable.codeGen()}.$key"
}

sealed trait Value {
  def render: String = ???
  def codeGen(): String = ???
}

sealed trait IndexedValue extends Value
final case class StringValue(v: String) extends Value {
  override def codeGen(): String = s""""$v""""
}
final case class BooleanValue(v: Boolean) extends Value {
  override def codeGen(): String = s"$v"
}
final case class IntValue(v: Int) extends Value {
  override def codeGen(): String = s"$v"
}
final case class MapValue(v: Map[String, Value]) extends IndexedValue {
  override def codeGen(): String = s"$v"
}
final case class ListValue(v: List[Value]) extends IndexedValue {
  override def codeGen(): String = s"$v"
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

object CodeGenUtil {
  def escape(s: String): String = s.flatMap(escapedChar)

  def escapedChar(ch: Char): String = ch match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"'  => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _    => if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt)
                 else              String.valueOf(ch)
  }
}
