package vlthr.tee.core
import scala.collection.mutable.{Map => MMap}
import scala.util.{Try, Success, Failure}
import vlthr.tee.parser.Liquid
import scala.util.control.NonFatal
import vlthr.tee.core.Errors._
import java.nio.file.Paths
import validation.Result

final case class BlockNode(node: List[Obj])(implicit val pctx: ParseContext)
    extends Obj {
  def render()(implicit ctx: Context) = {
    implicit val newScope = Context.createChild(ctx)
    val renders: Validated[List[String]] =
      Result.sequence(node.map(_.render()))
    renders.map(_.mkString)
  }
}

final case class OutputNode(expr: Expr)(implicit val pctx: ParseContext)
    extends Obj {
  def render()(implicit ctx: Context) = expr.render()
}

final case class TextNode(text: String)(implicit val pctx: ParseContext)
    extends Obj {
  def render()(implicit ctx: Context): Validated[String] = Result.valid(text)
}

trait TagNode extends Obj with ASTNode {
  def render()(implicit ctx: Context): Validated[String] = ???
}

final case class CaptureTag(id: String, value: Obj)(
    implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit ctx: Context): Validated[String] = {
    value.render().map { v: String =>
      ctx.mappings.put(id, StringValue(v))
      ""
    }
  }
}

final case class AssignTag(id: String, value: Expr)(
    implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit ctx: Context): Validated[String] = {
    value.eval() map { v: Value =>
      ctx.mappings.put(id, v)
      ""
    }
  }
}

final case class CommentTag()(implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit ctx: Context): Validated[String] =
    Result.valid("")
}

final case class CycleTag()(implicit val pctx: ParseContext) extends TagNode

final case class ForTag(id: String, expr: Expr, block: Obj)(
    implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit ctx: Context): Validated[String] = {
    val iterable: Validated[List[_]] = expr.eval().flatMap {
      case ListValue(l) => Result.valid(l)
      case _ => fail(InvalidIterable(expr))
    }
    iterable.flatMap { iterable =>
      // For each iteration of the loop, render the body
      val renders = iterable.map { v =>
        implicit val forCtx = Context.createChild(ctx)
        forCtx.mappings.put(id, Value.create(v))
        block.render()
      }
      // If successful, combine the bodies to a single string
      Result.sequence(renders.toList).map(_.mkString)
    }
  }
}

final case class BreakTag()(implicit val pctx: ParseContext) extends TagNode

final case class ContinueTag()(implicit val pctx: ParseContext) extends TagNode

final case class IfTag(condition: Expr,
                       thenBlock: Obj,
                       elsifs: List[(Expr, Obj)],
                       elseBlock: Option[Obj])(implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit ctx: Context): Validated[String] = {
    // val condErrors = (condition +: elsifs.collect { case (c, _) => c })
    //   .map(_.eval())
    //   .collect { case Failure(LiquidFailure(errors)) => errors }
    //   .flatten
    // val renderErrors =
    //   (elsifs.collect { case (_, b) => b } ++ elseBlock.toList)
    //     .map(_.render())
    //     .collect { case Failure(LiquidFailure(errors)) => errors }
    //     .flatten
    // val allErrors = condErrors ++ renderErrors
    // if (allErrors.size > 0) return fail(allErrors: _*)

    // TODO: Don't evaluate/render twice
    val cond = condition.eval()
    val render = thenBlock.render()

    val elsifResults = elsifs.map {
      case (c, block) => (c.eval(), block.render())
    }

    val elseResult = elseBlock.map(_.render())

    if (cond.get.truthy) render
    else {
      for ((c, block) <- elsifResults) if (c.get.truthy) return block
      elseResult.getOrElse(Result.valid(""))
    }
  }
}

final case class IncludeTag(filename: Expr)(implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit ctx: Context): Validated[String] = {
    filename.eval().flatMap {
      case StringValue(f) => {
        val path = Paths.get(ctx.includeDir, f);
        Liquid.parse(path.toString).flatMap(_.render())
      }
      case e => fail(InvalidInclude(this, e))
    }
  }
}

final case class RawTag(text: String)(implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit ctx: Context): Validated[String] = {
    Result.valid(text)
  }
}

final case class UnlessTag()(implicit val pctx: ParseContext) extends TagNode

final case class CustomTag(tag: Tag, args: List[Expr])(
    implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit ctx: Context): Validated[String] = {
    // val evaledArgs = Result.sequence(args.map(_.eval()))
    // evaledArgs.flatMap { args =>
    //   val t =
    //     implicit val parent = this
    //     tag.render(args)
    //     .recoverWith {
    //       case LiquidFailure(errors) =>
    //         fail(imbueFragments(errors))
    //       case NonFatal(e) => fail(UncaughtExceptionError(e))
    //       case e => fail(UncaughtExceptionError(e))
    //     }
    //   Result.fromTry(t)
    // }
    succeed("")
  }
}

final case class CaseTag(switchee: Expr,
                         whens: List[(Expr, Obj)],
                         els: Option[Obj])(implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit ctx: Context): Validated[String] = {
    val condErrors = (switchee +: whens.collect { case (c, _) => c })
      .map(_.eval())
      .collect { case Failure(LiquidFailure(errors)) => errors }
      .flatten
    val renderErrors =
      (whens.collect { case (_, b) => b } ++ els.toList)
        .map(_.render())
        .collect { case Failure(LiquidFailure(errors)) => errors }
        .flatten
    val allErrors = condErrors ++ renderErrors
    if (allErrors.size > 0) return Error.fail(allErrors: _*)

    val s = switchee.eval()

    val whenRenders = whens.map {
      case (c, block) => (c.eval(), block.render())
    }

    val elseRender = els.map(_.render())

    for ((c, block) <- whenRenders) if (c.get == s.get) return block
    elseRender.getOrElse(Result.valid(""))
  }
}
