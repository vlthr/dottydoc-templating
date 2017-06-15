package vlthr.tee.parser

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
    val expr = expVisitor.visitExpr(ctx.expr())
    OutputNode(expr)
  }
}
class LiquidExprVisitor extends LiquidBaseVisitor[Expr] {
  val template = new Template("")
  var rootExpr: Option[Expr] = None
  override def visitExpr(ctx: LiquidParser.ExprContext): Expr = {
    val term = visitTerm(ctx.term())
    rootExpr = Some(term)
    term
  }
  override def visitTerm(ctx: LiquidParser.TermContext): Expr = {
    ctx.getChild(0) match {
      case t: TerminalNode => {
        implicit val parseContext = ParseContext(t.getSymbol().getStartIndex(), t.getSymbol().getStopIndex(), template)
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
    }
  }
}
