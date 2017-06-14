package vlthr.tee.parser

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import vlthr.tee.core._

object Liquid {
  def parseExpr(node: String): Node = {
    val parser = makeParser(node)
    val tree = parser.expr()
    println(tree.toStringTree(parser))
    val visitor = new LiquidExprVisitor()
    tree.accept(visitor)
    OutputNode(visitor.rootExpr.get)
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

class LiquidExprVisitor extends LiquidBaseVisitor[Expr] {
  val template = new Template("")
  var rootExpr: Option[Expr] = None
  override def visitExpr(ctx: LiquidParser.ExprContext): Expr = {
    val term = visitTerm(ctx.term())
    rootExpr = Some(term)
    term
  }
  override def visitTerm(ctx: LiquidParser.TermContext): Expr = {
    val terminal = ctx.INT()
    println(terminal)
    implicit val parseContext = ParseContext(terminal.getSymbol().getStartIndex(), terminal.getSymbol().getStopIndex(), template)
    LiteralExpr(IntValue(terminal.getText().toInt))
  }
}
