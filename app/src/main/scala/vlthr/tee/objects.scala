package vlthr.tee.core
import scala.collection.mutable.Map
import scala.util.{Try, Success, Failure}
import vlthr.tee.parser.Liquid
import java.nio.file.Paths

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

final case class IfTag(
    condition: Expr,
    thenBlock: Node,
    elsifs: List[(Expr, Node)],
    elseBlock: Option[Node])(implicit val sourcePosition: SourcePosition)
    extends TagNode {
  override def render()(implicit evalContext: EvalContext): Try[String] = {
    val condErrors = (condition +: elsifs.collect { case (c, _) => c })
      .map(_.eval)
      .collect { case Failure(LiquidFailure(errors)) => errors }
      .flatten
    val renderErrors =
      (elsifs.collect { case (_, b) => b } ++ elseBlock.toList)
        .map(_.render)
        .collect { case Failure(LiquidFailure(errors)) => errors }
        .flatten
    val allErrors = condErrors ++ renderErrors
    if (allErrors.size > 0) return Error.fail(allErrors: _*)

    // TODO: Don't evaluate/render twice
    val cond = condition.eval
    val render = thenBlock.render

    val elsifResults = elsifs.map { case (c, block) => (c.eval, block.render) }

    val elseResult = elseBlock.map(_.render)

    if (cond.get.truthy) render
    else {
      for ((c, block) <- elsifResults) if (c.get.truthy) return block
      elseResult.getOrElse(Success(""))
    }
  }
}

final case class IncludeTag(filename: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends TagNode {
  override def render()(implicit evalContext: EvalContext): Try[String] = {
    Error
      .all(filename.eval) {
        case StringValue(f) => {
          val path = Paths.get(evalContext.includeDir, f);
          Liquid.parse(path.toString).flatMap(_.render)
        }
        case e => Error.invalidInclude(this, e)
      }
      .flatten
  }
}

final case class RawTag()(implicit val sourcePosition: SourcePosition)
    extends TagNode

final case class UnlessTag()(implicit val sourcePosition: SourcePosition)
    extends TagNode
