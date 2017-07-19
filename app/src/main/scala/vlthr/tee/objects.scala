package vlthr.tee.core
import scala.collection.mutable.{Map => MMap}
import scala.util.{Try, Success, Failure}
import vlthr.tee.parser.Liquid
import vlthr.tee.core.Error._
import java.nio.file.Paths

final case class BlockNode(node: List[Obj])(
    implicit val pctx: ParseContext)
    extends Obj {
  def render()(implicit evalContext: Context) = {
    implicit val newScope = Context.createChild(evalContext)
    val renders = node.map(_.render())
    Error.all(renders: _*)(r => r).map(_.mkString)
  }
}

final case class OutputNode(expr: Expr)(
    implicit val pctx: ParseContext)
    extends Obj {
  def render()(implicit evalContext: Context) = Error.all(expr.render) {
    e =>
      e
  }
}

final case class TextNode(text: String)(
    implicit val pctx: ParseContext)
    extends Obj {
  def render()(implicit evalContext: Context): Try[String] = Success(text)
}

trait TagNode extends Obj with ASTNode {
  def render()(implicit evalContext: Context): Try[String] = ???
}

final case class CaptureTag(id: String, value: Obj)(
    implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit evalContext: Context): Try[String] = {
    Error.all(value.render) { v: String =>
      evalContext.mappings.put(id, StringValue(v))
      ""
    }
  }
}

final case class AssignTag(id: String, value: Expr)(
    implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit evalContext: Context): Try[String] = {
    Error.all(value.eval) { v: Value =>
      evalContext.mappings.put(id, v)
      ""
    }
  }
}

final case class CaseTag()(implicit val pctx: ParseContext)
    extends TagNode

final case class CommentTag()(implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit evalContext: Context): Try[String] =
    Success("")
}

final case class CycleTag()(implicit val pctx: ParseContext)
    extends TagNode

final case class ForTag(id: String, expr: Expr, block: Obj)(
    implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit evalContext: Context): Try[String] = {
    val iterable: Try[List[_]] = expr.eval.flatMap {
      case ListValue(l) => Success(l)
      case _ => fail(InvalidIterable(expr))
    }
    Error
      .all(iterable) { iterable =>
        val renders = iterable.map { v =>
          implicit val forCtx = Context.createChild(evalContext)
          forCtx.mappings.put(id, Value.create(v))
          block.render
        }
        Error.all(renders: _*)(r => r).map(_.mkString)
      }
      .flatten
  }
}

final case class BreakTag()(implicit val pctx: ParseContext)
    extends TagNode

final case class ContinueTag()(implicit val pctx: ParseContext)
    extends TagNode

final case class IfTag(
    condition: Expr,
    thenBlock: Obj,
    elsifs: List[(Expr, Obj)],
    elseBlock: Option[Obj])(implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit evalContext: Context): Try[String] = {
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
    implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit evalContext: Context): Try[String] = {
    Error
      .all(filename.eval) {
        case StringValue(f) => {
          val path = Paths.get(evalContext.includeDir, f);
          Liquid.parse(path.toString).flatMap(_.render)
        }
        case e => fail(InvalidInclude(this, e))
      }
      .flatten
  }
}

final case class RawTag(text: String)(
    implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit evalContext: Context): Try[String] = {
    Success(text)
  }
}

final case class UnlessTag()(implicit val pctx: ParseContext)
    extends TagNode

object CustomTag {
  type TagConstructor = (ParseContext, List[Expr]) => TagNode
  val registry: MMap[String, TagConstructor] = MMap()
  def byName(name: String): Option[TagConstructor] = registry.get(name)
  def register(id: String, ctor: TagConstructor) = registry.put(id, ctor)
}
