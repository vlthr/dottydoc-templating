package com.vlthr.levee.core
import scala.collection.mutable.{Buffer, Map => MMap}
import scala.util.{Try, Success, Failure}
import com.vlthr.levee.parser.LeveeParser
import scala.util.control.NonFatal
import com.vlthr.levee.core.error._
import java.nio.file.Paths
import validation.Result
import scala.util.control.Breaks._

/** Represents a list of nodes */
final case class BlockNode(nodes: List[Obj])(implicit val pctx: ParseContext)
    extends Obj {
  def render()(implicit ctx: Context) = {
    implicit val newScope = Context.createChild(ctx)
    val renders: Buffer[Validated[String]] = scala.collection.mutable.Buffer()
    breakable {
      for (n <- nodes) {
        renders.append(n.render())
        if (newScope.executionState.breakWasHit || newScope.executionState.continueWasHit) {
          // Block contains a break/continue tag. Propagate it upwards.
          ctx.executionState.breakWasHit = newScope.executionState.breakWasHit
          ctx.executionState.continueWasHit =
            newScope.executionState.continueWasHit
          break
        }
      }
    }
    Result.sequence(renders).map(_.mkString)
  }
}

/** Any node that uses {{ double curly braces }} */
final case class OutputNode(expr: Expr)(implicit val pctx: ParseContext)
    extends Obj {
  def render()(implicit ctx: Context) = expr.render()
}

/** A node consisting of only raw text */
final case class TextNode(text: String)(implicit val pctx: ParseContext)
    extends Obj {
  def render()(implicit ctx: Context): Validated[String] = Result.valid(text)
}

/** Any node that uses {% percent curly braces %} */
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
      case _ => invalid(InvalidIterable(expr))
    }
    iterable.flatMap { iterable =>
      // For each iteration of the loop, render the body
      val renders: Buffer[Validated[String]] = Buffer()
      breakable {
        for (i <- iterable) {
          implicit val forCtx = Context.createChild(ctx)
          forCtx.mappings.put(id, Value.create(i))
          renders.append(block.render())
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

final case class ContinueTag()(implicit val pctx: ParseContext)
    extends TagNode {
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
    val condEval = condition.truthy()
    val elsifEvals = Result.sequence(elsifs.map {
      case (cond, body) => cond.truthy()
    }.toList)
    (condEval zip elsifEvals) flatMap { (c, eis) =>
      // Join all of the ifs to a (condition, body) form and find the first that matches
      val elseBranch = (true, TextNode(""))
      val elseifBranches = eis.zip(elsifs.map(_._2))
      val allBranches
        : Seq[(Boolean, Obj)] = ((c, thenBlock) +: elseifBranches :+ elseBranch)
      allBranches
        .find { case (cond, body) => cond }
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
        val path = Paths.get(ctx.config.includeDir, f)
        LeveeParser.parse(path.toString).flatMap(_.render())
      }
      case e => invalid(InvalidInclude(this, e))
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
    val argsVal = Result.sequence(args.map(_.eval()))

    try {
      argsVal flatMap { args =>
        imbueFragments(tag.apply(args))
      }
    } catch {
      case NonFatal(err) => invalid(UncaughtExceptionError(err))
    }
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
