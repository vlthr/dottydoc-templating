package vlthr.tee.core
import scala.collection.mutable.Map
import scala.util.{Try, Success, Failure}

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
  def render()(implicit evalContext: EvalContext) = Error.all(expr.render) {
    e =>
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
    Error
      .all(iterable) { iterable =>
        val renders = iterable.map { v =>
          implicit val forCtx = EvalContext.createChild(evalContext)
          forCtx.mappings.put(id, Value.create(v))
          block.render
        }
        Error.all(renders: _*)(r => r).map(_.mkString)
      }
      .flatten
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
