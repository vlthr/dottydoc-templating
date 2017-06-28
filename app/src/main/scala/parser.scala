package vlthr.tee.parser

import scala.collection.JavaConverters._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import scala.collection.mutable.Buffer
import vlthr.tee.core._
import scala.util.{Try, Success, Failure}

object Liquid {
  def parseExpr(node: String): Expr = {
    val parser = makeParser(node, lexerMode = Object())
    val tree = parser.expr()
    val visitor = new LiquidExprVisitor(SourceFile(node))
    tree.accept(visitor)
  }

  def parseNode(node: String): Node = {
    val parser = makeParser(node)
    val tree = parser.node()
    val visitor = new LiquidNodeVisitor(SourceFile(node))
    tree.accept(visitor)
  }

  def parseTemplate(node: String): Node = {
    val parser = makeParser(node)
    val tree = parser.block()
    val visitor = new LiquidNodeVisitor(SourceFile(node))
    tree.accept(visitor)
  }

  def getParseTree(node: String): String = {
    val input = new ANTLRInputStream(node)
    val lexer = new LiquidLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new LiquidParser(tokens)
    lexer.removeErrorListeners();
    parser.removeErrorListeners();
    val tree = parser.block()
    tree.toStringTree(parser)
  }

  def parse(node: String): Try[Node] = {
    val input = new ANTLRInputStream(node)
    val lexer = new LiquidLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new LiquidParser(tokens)
    lexer.removeErrorListeners();
    val errors = new GatherErrors()
    lexer.addErrorListener(errors);
    parser.removeErrorListeners();
    parser.addErrorListener(errors);
    val tree = parser.block()
    val result = tree.accept(new LiquidNodeVisitor(SourceFile(node)))
    if (errors.errors.size != 0) Failure(LiquidFailure(errors.errors.toList))
    else Success(result)
  }

  sealed trait LexerMode
  final case class Default() extends LexerMode
  final case class Object() extends LexerMode

  def makeParser(text: String,
                 lexerMode: LexerMode = Default()): LiquidParser = {
    val input = new ANTLRInputStream(text)

    val lexer = new LiquidLexer(input)
    lexerMode match {
      case Object() => lexer.pushMode(LiquidLexer.Object)
      case Default() =>
    }
    val tokens = new CommonTokenStream(lexer)
    val parser = new LiquidParser(tokens)
    parser.setErrorHandler(new BailErrorStrategy());
    parser
  }
}

class GatherErrors extends BaseErrorListener {
  val errors = Buffer[Error]()

  override def syntaxError(recognizer: Recognizer[_, _],
                           offendingSymbol: Object,
                           line: Int,
                           charPositionInLine: Int,
                           msg: String,
                           e: RecognitionException) = {
    errors.append(ParseError(msg))
  }
}

class LiquidNodeVisitor(template: SourceFile) extends LiquidParserBaseVisitor[Node] {

  override def visitNode(ctx: LiquidParser.NodeContext): Node = {
    if (ctx.tag() != null) {
      visitTag(ctx.tag())
    } else if (ctx.output() != null) {
      visitOutput(ctx.output())
    } else if (ctx.TEXT() != null) {
      visitText(ctx.TEXT())
    } else {
      throw new Exception("Missing node definition");
    }
  }
  def visitText(t: TerminalNode): Node = {
    implicit val sourcePosition = SourcePosition(t.getSymbol().getStartIndex(),
                                             t.getSymbol().getStopIndex(),
                                             template)

    TextNode(t.getText())
  }

  override def visitOutput(ctx: LiquidParser.OutputContext): Node = {
    val expVisitor = new LiquidExprVisitor(template)
    val t = ctx.expr()
    val expr = expVisitor.visitExpr(t)
    implicit val sourcePosition = SourcePosition(ctx.start.getStartIndex(),
                                             ctx.stop.getStopIndex(),
                                             template)
    OutputNode(expr)
  }

  override def visitTag(ctx: LiquidParser.TagContext): Node = {
    implicit val sourcePosition = SourcePosition(ctx.start.getStartIndex(),
                                             ctx.stop.getStopIndex(),
                                             template)
    if (ctx.ifTag() != null) {
      val expr =
        new LiquidExprVisitor(template).visitExpr(ctx.ifTag().ifStart().expr())
      val block = visitBlock(ctx.ifTag().block())
      IfTag(expr, block)
    } else if (ctx.forTag() != null) {
      val id = ctx.forTag().forStart().id().getText()
      val expr =
        new LiquidExprVisitor(template).visitExpr(ctx.forTag().forStart().expr())
      val block = visitBlock(ctx.forTag().block())
      ForTag(id, expr, block)
    } else if (ctx.assignTag() != null) {
      val id = ctx.assignTag().id().getText()
      val expr =
        new LiquidExprVisitor(template).visitExpr(ctx.assignTag().expr())
      AssignTag(id, expr)

    } else throw new Exception("Unknown node type")
  }

  override def visitBlock(ctx: LiquidParser.BlockContext): Node = {
    implicit val sourcePosition = SourcePosition(ctx.start.getStartIndex(),
                                             ctx.stop.getStopIndex(),
                                             template)
    BlockNode(ctx.node().asScala.toList.map(n => visitNode(n)))
  }
}

class LiquidArgsVisitor(template: SourceFile) extends LiquidParserBaseVisitor[List[Expr]] {
  override def visitArgs(ctx: LiquidParser.ArgsContext): List[Expr] = {
    ctx
      .arglist()
      .expr()
      .asScala
      .map(ectx => {
        implicit val sourcePosition = SourcePosition(ectx.start.getStartIndex(),
                                                 ectx.stop.getStopIndex(),
                                                 template)
        new LiquidExprVisitor(template).visitExpr(ectx)
      })
      .toList
  }
}

class LiquidExprVisitor(template: SourceFile) extends LiquidParserBaseVisitor[Expr] {
  override def visitExpr(ctx: LiquidParser.ExprContext): Expr = {
    implicit val sourcePosition = SourcePosition(ctx.start.getStartIndex(),
                                             ctx.stop.getStopIndex(),
                                             template)
    if (ctx.FILTER() != null) {
      val args =
        if (ctx.args() != null) new LiquidArgsVisitor(template).visitArgs(ctx.args())
        else Nil
      FilterExpr(visitExpr(ctx.expr(0)),
                 Filter.byName(ctx.id().getText()),
                 args)
    } else if (ctx.DOTINDEX() != null) {
      DotExpr(visitExpr(ctx.expr(0)), ctx.id().getText())
    } else if (ctx.STARTINDEX() != null) {
      IndexExpr(visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
    } else if (ctx.AND() != null) {
      AndExpr(visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
    } else if (ctx.OR() != null) {
      OrExpr(visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
    } else if (ctx.EQ() != null) {
      EqExpr(visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
    } else if (ctx.NEQ() != null) {
      NEqExpr(visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
    } else if (ctx.LT() != null) {
      LtExpr(visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
    } else if (ctx.LEQ() != null) {
      LEqExpr(visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
    } else if (ctx.GT() != null) {
      GtExpr(visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
    } else if (ctx.GEQ() != null) {
      GEqExpr(visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
    } else {
      val term = visitTerm(ctx.term())
      term
    }
  }

  override def visitTerm(ctx: LiquidParser.TermContext): Expr = {
    implicit val sourcePosition = SourcePosition(ctx.start.getStartIndex(),
                                             ctx.start.getStopIndex(),
                                             template)
    ctx.getChild(0) match {
      case t: TerminalNode => {
        if (ctx.INT() != null) {
          LiteralExpr(IntValue(t.getText().toInt))
        } else if (ctx.STRSINGLE() != null || ctx.STRDOUBLE() != null) {
          LiteralExpr(
            StringValue(t.getText().substring(1, t.getText().size - 1)))
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
