package com.vlthr.levee.core
import scala.collection.mutable.{Map => MMap}
import scala.util.{Try, Success, Failure}
import com.vlthr.levee.parser.Liquid
import scala.util.control.NonFatal
import com.vlthr.levee.core.error._
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
    val condEval = condition.eval()
    val thenEval = thenBlock.render()
    val elsifEvals = Result.sequence(elsifs.map {
      case (cond, body) => cond.eval() zip body.render()
    })
    val elseEval: Validated[Option[String]] = elseBlock
      .map(_.render())
      .map(r => r.map(s => Some(s)))
      .getOrElse(valid(None))
    (condEval and thenEval and elsifEvals and elseEval) {
      case (c, t, eis, e) =>
        // Join all of the ifs to a (condition, body) form and find the first that matches
        (((c, t) +: eis) ++ e.map(r => (BooleanValue(true), r)).toList)
          .find { case (cond, body) => cond.truthy }
          .get
          ._2
    }
  }
}

final case class IncludeTag(filename: Expr)(implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit ctx: Context): Validated[String] = {
    filename.eval().flatMap {
      case StringValue(f) => {
        val path = Paths.get(ctx.includeDir, f)
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
    valid("")
  }
}

final case class CaseTag(switchee: Expr,
                         whens: List[(Expr, Obj)],
                         els: Option[Obj])(implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit ctx: Context): Validated[String] = {
    val switcheeEval = switchee.eval()
    val whenEvals = Result.sequence(whens.map {
      case (comparison, body) => comparison.eval() zip body.render()
    })
    // If there is an else, invert it so that it is a Validated[Option[String]]
    // That way it can be used with the Result combinators
    val elseEval: Validated[Option[String]] = els
      .map(_.render())
      .map(r => r.map(s => Some(s)))
      .getOrElse(valid(None))
    (switcheeEval and whenEvals and elseEval) {
      case (s, ws, e) =>
        // Add else clause to the end of the when cases, guaranteed to match the switchee
        (ws ++ e.map(r => (s, r)).toList)
          .find { case (comparison, body) => comparison == s }
          .get
          ._2
    }
  }
}
