package vlthr.tee.parser

import scala.collection.JavaConverters._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import vlthr.tee.core._

object Liquid {
  def parseExpr(node: String): Expr = {
    val parser = makeParser(node)
    val tree = parser.expr()
    println(tree.toStringTree(parser))
    val visitor = new LiquidExprVisitor()
    tree.accept(visitor)
    visitor.rootExpr.get
  }
  def parseNode(node: String): Node = {
    val parser = makeParser(node)
    val tree = parser.node()
    println(tree.toStringTree(parser))
    val visitor = new LiquidNodeVisitor()
    tree.accept(visitor)
  }
  def makeParser(text: String): LiquidParser = {
    // create a CharStream that reads from standard input
    val input = new ANTLRInputStream(text)
    
    // create a lexer that feeds off of input CharStream
    val lexer = new LiquidLexer(input)

    // create a buffer of tokens pulled from the lexer
    val tokens = new CommonTokenStream(lexer)

    // create a parser that feeds off the tokens buffer
    val parser = new LiquidParser(tokens)
    parser.setErrorHandler(new BailErrorStrategy());
    parser
  }
}

class LiquidNodeVisitor extends LiquidBaseVisitor[Node] {
  val template = new Template("")
  override def visitNode(ctx: LiquidParser.NodeContext): Node = {
    if (ctx.tag() != null) {
      ???
    } else if (ctx.output() != null) {
      visitOutput(ctx.output())
    } else throw new Exception("Unknown node type")
  }
  override def visitOutput(ctx: LiquidParser.OutputContext): Node = {
    val expVisitor = new LiquidExprVisitor()
    val t = ctx.expr()
    val expr = expVisitor.visitExpr(t)
    implicit val parseContext = ParseContext(ctx.start.getStartIndex(), ctx.stop.getStopIndex(), template)
    OutputNode(expr)
  }
}

class LiquidArgsVisitor extends LiquidBaseVisitor[List[Expr]] {
  val template = new Template("")

  override def visitArgs(ctx: LiquidParser.ArgsContext): List[Expr] = {
    ctx.arglist().expr().asScala.map(ectx => {
                               implicit val parseContext = ParseContext(ectx.start.getStartIndex(), ectx.stop.getStopIndex(), template)
                               new LiquidExprVisitor().visitExpr(ectx)
                             }).toList
  }
}
class LiquidExprVisitor extends LiquidBaseVisitor[Expr] {
  val template = new Template("")
  var rootExpr: Option[Expr] = None
  override def visitExpr(ctx: LiquidParser.ExprContext): Expr = {
    if (ctx.FILTER() != null) {
      implicit val parseContext = ParseContext(ctx.start.getStartIndex(), ctx.stop.getStopIndex(), template)
      val args = if (ctx.args() != null) new LiquidArgsVisitor().visitArgs(ctx.args())
                 else Nil
      FilterExpr(visitExpr(ctx.expr()), Filter.byName(ctx.id().getText()), args)
    } else {
      val term = visitTerm(ctx.term())
      rootExpr = Some(term)
      term
    }
  }

  override def visitTerm(ctx: LiquidParser.TermContext): Expr = {
    implicit val parseContext = ParseContext(ctx.start.getStartIndex(), ctx.start.getStopIndex(), template)
    ctx.getChild(0) match {
      case t: TerminalNode => {
        if (ctx.INT() != null) {
          LiteralExpr(IntValue(t.getText().toInt))
        } else if (ctx.STRSINGLE() != null || ctx.STRDOUBLE() != null) {
          println(t.getText())
          LiteralExpr(StringValue(t.getText().substring(1, t.getText().size-1)))
        } else if (ctx.TRUE() != null) {
          LiteralExpr(BooleanValue(true))
        } else if (ctx.FALSE() != null) {
          LiteralExpr(BooleanValue(false))
        } else throw new Exception("Unknown term: " + t + " in context " + ctx)
      }
      case ictx: LiquidParser.IdContext => {
        VariableUseExpr(ctx.id().getText())
      }
    }
  }
}

