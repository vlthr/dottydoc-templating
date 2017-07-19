package vlthr.tee.parser

import scala.collection.JavaConverters._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import org.antlr.v4.runtime.misc.Interval
import scala.collection.mutable.{Buffer, Map => MMap}
import vlthr.tee.core._
import vlthr.tee.core.Error._
import vlthr.tee.filters._
import vlthr.tee.util.Util
import scala.util.{Try, Success, Failure}

object Liquid {
  def makeContext(c: ParserRuleContext, template: SourceFile) = {
    val stop = if (c.stop != null) c.stop else c.start
    val sourcePosition =
      SourcePosition(c.start.getStartIndex(), stop.getStopIndex(), template)
    ParseContext(sourcePosition)
  }
  def parseExpr(node: String): Expr = {
    val parser = makeParser(node, lexerMode = Object())
    val tree = parser.expr()
    implicit val ctx = Context.createNew()
    val visitor = new LiquidExprVisitor(SourceFile(node, "./"))
    tree.accept(visitor)
  }

  def parseNode(node: String): Obj = {
    val parser = makeParser(node)
    val tree = parser.node()
    implicit val ctx = Context.createNew()
    val visitor = new LiquidNodeVisitor(SourceFile(node, "./"))
    tree.accept(visitor)
  }

  def parseTemplate(node: String): Obj = {
    val parser = makeParser(node)
    val tree = parser.template()
    implicit val ctx = Context.createNew()
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

  def parse(file: SourceFile)(implicit ctx: Context): Try[Obj] = {
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

  def parse(path: String)(implicit ctx: Context): Try[Obj] =
    parse(SourceFile(Util.readWholeFile(path), path))

  def render(path: String,
             params: Map[String, Any],
             includeDir: String): Try[String] = {
    implicit val c: Context = Context.createNew.withParams(params).withIncludeDir(includeDir)
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
  override def reportNoViableAlternative(parser: Parser,
                                         e: NoViableAltException) = {
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
    val length =
      if (offendingSymbol != null)
        offendingSymbol.asInstanceOf[Token].getText.size
      else 0
    implicit val sourcePosition: SourcePosition =
      SourcePosition.fromLine(template, line - 1, charPositionInLine, length)
    implicit val pc = ParseContext(sourcePosition)
    errors.append(ParseError(recognizer, offendingSymbol, msg, e))
  }
}

class LiquidNodeVisitor(template: SourceFile)(implicit val ctx: Context)
    extends LiquidParserBaseVisitor[Obj] {

  override def visitNode(c: LiquidParser.NodeContext): Obj = {
    if (c.tag() != null) {
      visitTag(c.tag())
    } else if (c.output() != null) {
      visitOutput(c.output())
    } else if (c.TEXT() != null) {
      visitText(c.TEXT())
    } else {
      throw new Exception("Missing node definition");
    }
  }

  def visitText(t: TerminalNode): Obj = {
    val sourcePosition = SourcePosition(t.getSymbol().getStartIndex(),
                                                 t.getSymbol().getStopIndex(),
                                                 template)
    implicit val pc = ParseContext(sourcePosition)
    TextNode(t.getText())
  }

  override def visitOutput(c: LiquidParser.OutputContext): Obj = {
    implicit val pc = Liquid.makeContext(c, template)
    val output_expr = c.output_expr()
    OutputNode(visitOutputExpr(output_expr))
  }

  def visitOutputExpr(c: LiquidParser.Output_exprContext): Expr = {
    implicit val pc = Liquid.makeContext(c, template)
    val expVisitor = new LiquidExprVisitor(template)
    if (c.FILTER() != null) {
      val args =
        if (c.COLON() != null)
          new LiquidArgsVisitor(template).visitArglist(c.arglist())
        else Nil
      val filter = ctx.getFilter(c.id.getText)
      FilterExpr(visitOutputExpr(c.output_expr()), filter, args)
    } else {
      expVisitor.visitExpr(c.expr())
    }
  }

  override def visitTag(c: LiquidParser.TagContext): Obj = {
    implicit val pc = Liquid.makeContext(c, template)
    if (c.ifTag() != null) {
      val ev = new LiquidExprVisitor(template)
      val expr =
        ev.visitExpr(c.ifTag().ifStart().expr())
      val block = visitBlock(c.ifTag().block())
      val elsifs =
        if (c.ifTag().elsif() != null)
          c
            .ifTag()
            .elsif()
            .asScala
            .map(elsif =>
              (ev.visitExpr(elsif.expr()), visitBlock(elsif.block())))
            .toList
        else Nil
      val els =
        if (c.ifTag().els() != null)
          Some(visitBlock(c.ifTag().els().block()))
        else None
      IfTag(expr, block, elsifs, els)
    } else if (c.forTag() != null) {
      val id = c.forTag().forStart().id().getText()
      val expr = visitOutputExpr(c.forTag().forStart().output_expr())
      val block = visitBlock(c.forTag().block())
      ForTag(id, expr, block)
    } else if (c.assignTag() != null) {
      val id = c.assignTag().id().getText()
      val expr =
        new LiquidExprVisitor(template).visitExpr(c.assignTag().expr())
      AssignTag(id, expr)
    } else if (c.captureTag() != null) {
      val id = c.captureTag().id().getText()
      val value = visitBlock(c.captureTag().block())
      CaptureTag(id, value)
    } else if (c.includeTag() != null) {
      val expr =
        new LiquidExprVisitor(template).visitExpr(c.includeTag().expr())
      IncludeTag(expr)
    } else if (c.commentTag() != null) {
      CommentTag()
    } else if (c.rawTag() != null) {
      val stop =
        if (c.rawTag.any.stop != null) c.rawTag.any.stop
        else c.rawTag.any.start
      RawTag(
        c.rawTag.start
          .getInputStream()
          .getText(new Interval(c.rawTag.any.start.getStartIndex,
                                c.rawTag.any.stop.getStopIndex)))
    } else if (c.customTag != null) {
      val id = c.customTag.id.getText
      val args =
        if (c.customTag.arglist != null)
          new LiquidArgsVisitor(template).visitArglist(c.customTag.arglist)
        else Nil
      CustomTag.byName(id).map(ctor => ctor(pc, args)).getOrElse {
        throw InvalidTagIdException((InvalidTagId(id)))
      }
    } else {
      throw MalformedTagException(MalformedTag())
    }
  }

  override def visitTemplate(c: LiquidParser.TemplateContext): Obj = {
    visitBlock(c.block())
  }

  override def visitBlock(c: LiquidParser.BlockContext): Obj = {
    implicit val pc = Liquid.makeContext(c, template)
    BlockNode(c.node().asScala.toList.map(n => visitNode(n)))
  }
}

class LiquidArgsVisitor(template: SourceFile)(implicit val ctx: Context)
    extends LiquidParserBaseVisitor[List[Expr]] {
  override def visitArglist(c: LiquidParser.ArglistContext): List[Expr] = {
    c
      .expr()
      .asScala
      .map(ec => {
        implicit val pc = Liquid.makeContext(c, template)
        new LiquidExprVisitor(template).visitExpr(ec)
      })
      .toList
  }
}

class LiquidExprVisitor(template: SourceFile)(implicit val ctx: Context)
    extends LiquidParserBaseVisitor[Expr] {
  override def visitExpr(c: LiquidParser.ExprContext): Expr = {
    implicit val pc = Liquid.makeContext(c, template)
    if (c.DOTINDEX() != null) {
      DotExpr(visitExpr(c.expr(0)), c.id().getText())
    } else if (c.STARTINDEX() != null) {
      IndexExpr(visitExpr(c.expr(0)), visitExpr(c.expr(1)))
    } else if (c.AND() != null) {
      AndExpr(visitExpr(c.expr(0)), visitExpr(c.expr(1)))
    } else if (c.OR() != null) {
      OrExpr(visitExpr(c.expr(0)), visitExpr(c.expr(1)))
    } else if (c.EQ() != null) {
      EqExpr(visitExpr(c.expr(0)), visitExpr(c.expr(1)))
    } else if (c.NEQ() != null) {
      NEqExpr(visitExpr(c.expr(0)), visitExpr(c.expr(1)))
    } else if (c.LT() != null) {
      LtExpr(visitExpr(c.expr(0)), visitExpr(c.expr(1)))
    } else if (c.LEQ() != null) {
      LEqExpr(visitExpr(c.expr(0)), visitExpr(c.expr(1)))
    } else if (c.GT() != null) {
      GtExpr(visitExpr(c.expr(0)), visitExpr(c.expr(1)))
    } else if (c.GEQ() != null) {
      GEqExpr(visitExpr(c.expr(0)), visitExpr(c.expr(1)))
    } else {
      val term = visitTerm(c.term())
      term
    }
  }

  override def visitTerm(c: LiquidParser.TermContext): Expr = {
    implicit val pc = Liquid.makeContext(c, template)
    c.getChild(0) match {
      case t: TerminalNode => {
        if (c.INT() != null) {
          LiteralExpr(IntValue(t.getText().toInt))
        } else if (c.STRSINGLE() != null || c.STRDOUBLE() != null) {
          LiteralExpr(
            StringValue(t.getText().substring(1, t.getText().size - 1)))
        } else if (c.TRUE() != null) {
          LiteralExpr(BooleanValue(true))
        } else if (c.FALSE() != null) {
          LiteralExpr(BooleanValue(false))
        } else throw new Exception("Unknown term: " + t + " in context " + c)
      }
      case ic: LiquidParser.IdContext => {
        VariableUseExpr(c.id().getText())
      }
    }
  }
}
