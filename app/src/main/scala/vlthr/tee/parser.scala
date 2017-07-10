package vlthr.tee.parser

import scala.collection.JavaConverters._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import scala.collection.mutable.{Buffer, Map => MMap}
import vlthr.tee.core._
import vlthr.tee.core.Error._
import vlthr.tee.filters._
import vlthr.tee.util.Util
import scala.util.{Try, Success, Failure}

object Liquid {
  def parseExpr(node: String): Expr = {
    val parser = makeParser(node, lexerMode = Object())
    val tree = parser.expr()
    val visitor = new LiquidExprVisitor(SourceFile(node, "./"))
    tree.accept(visitor)
  }

  def parseNode(node: String): Node = {
    val parser = makeParser(node)
    val tree = parser.node()
    val visitor = new LiquidNodeVisitor(SourceFile(node, "./"))
    tree.accept(visitor)
  }

  def parseTemplate(node: String): Node = {
    val parser = makeParser(node)
    val tree = parser.template()
    val visitor = new LiquidNodeVisitor(SourceFile(node, "./"))
    tree.accept(visitor)
  }

  def getParseTree(node: String): String = {
    val input = new ANTLRInputStream(node)
    val lexer = new LiquidLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new LiquidParser(tokens)
    lexer.removeErrorListeners();
    parser.removeErrorListeners();
    val tree = parser.template()
    tree.toStringTree(parser)
  }

  def parse(file: SourceFile): Try[Node] = {
    val input = new ANTLRInputStream(file.body)
    val lexer = new LiquidLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new LiquidParser(tokens)
    val errors = new GatherErrors(file)
    lexer.removeErrorListeners();
    lexer.addErrorListener(errors);
    parser.removeErrorListeners();
    parser.addErrorListener(errors);
    val tree = parser.template()
    val result = tree.accept(new LiquidNodeVisitor(file))
    if (errors.errors.size != 0) Failure(LiquidFailure(errors.errors.toList))
    else Success(result)
  }

  def parse(path: String): Try[Node] =
    parse(SourceFile(Util.readWholeFile(path), path))

  def render(path: String, params: Map[String, Any], includeDir: String): Try[String] = {
    val p: Map[String, Value] = params.map {
                                           case (k: String, v: Any) => (k, Value.create(v))
                                         }
    implicit val ctx: EvalContext = EvalContext.createNew(p, includeDir)
    parse(path).flatMap(_.render)
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

class ErrorStrategy extends DefaultErrorStrategy {
  override def reportNoViableAlternative(parser: Parser, e: NoViableAltException) = {
    val msg = s"No viable alternative found"
    parser.notifyErrorListeners(e.getOffendingToken(), msg, e)
  }
  override def reportUnwantedToken(parser: Parser) = {
    println("AOEU")
    val msg = s"Unwanted token"
    parser.notifyErrorListeners(msg)
  }
}

class GatherErrors(template: SourceFile) extends BaseErrorListener {
  val errors = Buffer[Error]()

  override def syntaxError(recognizer: Recognizer[_, _],
                           offendingSymbol: Object,
                           line: Int,
                           charPositionInLine: Int,
                           msg: String,
                           e: RecognitionException) = {
    // TODO: Get begin/end from line/char
    val length = if (offendingSymbol != null) offendingSymbol.asInstanceOf[Token].getText.size else 0
    implicit val sourcePosition: SourcePosition = SourcePosition.fromLine(template, line-1, charPositionInLine, length)
    errors.append(ParseError(recognizer, offendingSymbol, msg, e))
  }
}

class LiquidNodeVisitor(template: SourceFile)
    extends LiquidParserBaseVisitor[Node] {

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
    implicit val sourcePosition = SourcePosition(ctx.start.getStartIndex(),
                                                 ctx.stop.getStopIndex(),
                                                 template)
    val output_expr = ctx.output_expr()
    OutputNode(visitOutputExpr(output_expr))
  }

  def visitOutputExpr(ctx: LiquidParser.Output_exprContext): Expr = {
    implicit val sourcePosition = SourcePosition(ctx.start.getStartIndex(),
                                                 ctx.stop.getStopIndex(),
                                                 template)
    val expVisitor = new LiquidExprVisitor(template)
    if (ctx.FILTER() != null) {
      val args =
        if (ctx.args() != null)
          new LiquidArgsVisitor(template).visitArgs(ctx.args())
        else Nil
      FilterExpr(visitOutputExpr(ctx.output_expr()),
                 Filter.byName(ctx.id().getText()),
                 args)
    } else {
      expVisitor.visitExpr(ctx.expr())
    }
  }

  override def visitTag(ctx: LiquidParser.TagContext): Node = {
    implicit val sourcePosition = SourcePosition(ctx.start.getStartIndex(),
                                                 ctx.stop.getStopIndex(),
                                                 template)
    if (ctx.ifTag() != null) {
      val ev = new LiquidExprVisitor(template)
      val expr =
        ev.visitExpr(ctx.ifTag().ifStart().expr())
      val block = visitBlock(ctx.ifTag().block())
      val elsifs =
        if (ctx.ifTag().elsif() != null)
          ctx
            .ifTag()
            .elsif()
            .asScala
            .map(elsif =>
              (ev.visitExpr(elsif.expr()), visitBlock(elsif.block())))
            .toList
        else Nil
      val els =
        if (ctx.ifTag().els() != null)
          Some(visitBlock(ctx.ifTag().els().block()))
        else None
      IfTag(expr, block, elsifs, els)
    } else if (ctx.forTag() != null) {
      val id = ctx.forTag().forStart().id().getText()
      val expr = visitOutputExpr(ctx.forTag().forStart().output_expr())
      val block = visitBlock(ctx.forTag().block())
      ForTag(id, expr, block)
    } else if (ctx.assignTag() != null) {
      val id = ctx.assignTag().id().getText()
      val expr =
        new LiquidExprVisitor(template).visitExpr(ctx.assignTag().expr())
      AssignTag(id, expr)
    } else if (ctx.captureTag() != null) {
      val id = ctx.captureTag().id().getText()
      val value = visitBlock(ctx.captureTag().block())
      CaptureTag(id, value)
    } else if (ctx.includeTag() != null) {
      val expr =
        new LiquidExprVisitor(template).visitExpr(ctx.includeTag().expr())
      IncludeTag(expr)
    } else if (ctx.rawTag() != null) {
      RawTag(ctx.rawTag().non_tag_start().getText())
    } else throw new Exception("Unknown node type")
  }

  override def visitTemplate(ctx: LiquidParser.TemplateContext): Node = {
    visitBlock(ctx.block())
  }

  override def visitBlock(ctx: LiquidParser.BlockContext): Node = {
    val stop = if (ctx.stop != null) ctx.stop else ctx.start
    implicit val sourcePosition = SourcePosition(ctx.start.getStartIndex(),
                                                 stop.getStopIndex(),
                                                 template)
    BlockNode(ctx.node().asScala.toList.map(n => visitNode(n)))
  }
}

class LiquidArgsVisitor(template: SourceFile)
    extends LiquidParserBaseVisitor[List[Expr]] {
  override def visitArgs(ctx: LiquidParser.ArgsContext): List[Expr] = {
    ctx
      .arglist()
      .expr()
      .asScala
      .map(ectx => {
        implicit val sourcePosition =
          SourcePosition(ectx.start.getStartIndex(),
                         ectx.stop.getStopIndex(),
                         template)
        new LiquidExprVisitor(template).visitExpr(ectx)
      })
      .toList
  }
}

class LiquidExprVisitor(template: SourceFile)
    extends LiquidParserBaseVisitor[Expr] {
  override def visitExpr(ctx: LiquidParser.ExprContext): Expr = {
    implicit val sourcePosition = SourcePosition(ctx.start.getStartIndex(),
                                                 ctx.stop.getStopIndex(),
                                                 template)
    if (ctx.DOTINDEX() != null) {
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
