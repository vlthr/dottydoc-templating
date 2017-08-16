package com.vlthr.levee.core
import scala.collection.mutable.{Buffer, Map => MMap}
import scala.util.{Try, Success, Failure}
import com.vlthr.levee.parser.Liquid
import scala.util.control.NonFatal
import com.vlthr.levee.core.error._
import java.nio.file.Paths
import validation.Result
import scala.util.control.Breaks._

final case class BlockNode(nodes: List[Obj])(implicit val pctx: ParseContext)
    extends Obj {
  def render()(implicit ctx: Context) = {
    implicit val newScope = Context.createChild(ctx)
    val renders: Buffer[Validated[String]] = scala.collection.mutable.Buffer()
    breakable {
      for (n <- nodes) {
        renders.append(n.render()(newScope))
        if (newScope.executionState.breakWasHit || newScope.executionState.continueWasHit) {
          // Block contains a break/continue tag. Propagate it upwards.
          ctx.executionState.breakWasHit = newScope.executionState.breakWasHit
          ctx.executionState.continueWasHit = newScope.executionState.continueWasHit
          break
        }
      }
    }
    Result.sequence(renders).map(_.mkString)
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
      val renders: Buffer[Validated[String]] = Buffer()
      breakable {
        for (i <- iterable) {
          implicit val forCtx = Context.createChild(ctx)
          forCtx.mappings.put(id, Value.create(i))
          renders.append(block.render()(forCtx))
          if (forCtx.executionState.breakWasHit) {
            forCtx.executionState.breakWasHit = false
            break
          }
          forCtx.executionState.continueWasHit = false
        }
      }
      // If successful, combine the bodies to a single string
      Result.sequence(renders).map(_.mkString)
    }
  }
}

final case class BreakTag()(implicit val pctx: ParseContext) extends TagNode {
  override def render()(implicit ctx: Context): Validated[String] = {
    ctx.executionState.breakWasHit = true
    Result.valid("")
  }
}

final case class ContinueTag()(implicit val pctx: ParseContext) extends TagNode {
  override def render()(implicit ctx: Context): Validated[String] = {
    ctx.executionState.continueWasHit = true
    Result.valid("")
  }
}

final case class IfTag(condition: Expr,
                       thenBlock: Obj,
                       elsifs: List[(Expr, Obj)],
                       elseBlock: Option[Obj])(implicit val pctx: ParseContext)
    extends TagNode {
  override def render()(implicit ctx: Context): Validated[String] = {
    val condEval = condition.eval()
    val elsifEvals = Result.sequence(elsifs.map {
                                       case (cond, body) => cond.eval()
                                     }.toList)
    (condEval zip elsifEvals) flatMap { case (c: Value, eis: List[Value]) =>
        // Join all of the ifs to a (condition, body) form and find the first that matches
      val elseBranch = (BooleanValue(true), TextNode(""))
      val elseifBranches = eis.zip(elsifs.map(_._2))
      val allBranches: Seq[(Value, Obj)]= ((c, thenBlock) +: elseifBranches :+ elseBranch)
      allBranches
        .find { case (cond, body) => cond.truthy }
        .get
        ._2
        .render()
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
      case (comparison, body) => comparison.eval()
    })
    (switcheeEval zip whenEvals) flatMap {
      case (s, ws) =>
        // Add else clause to the end of the when cases, guaranteed to match the switchee
        val elseBranch: (Value, Obj) = (s, els.getOrElse(TextNode("")))
        val allBranches = (ws.zip(whens.map(_._2)) :+ elseBranch)
        allBranches
          .find { case (comparison, body) => comparison == s }
          .get
          ._2
          .render()
    }
  }
}
