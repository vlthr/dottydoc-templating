package com.vlthr.levee.parser

import scala.collection.JavaConverters._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import org.antlr.v4.runtime.misc.Interval
import scala.collection.mutable.{Buffer, Map => MMap}
import com.vlthr.levee.core._
import com.vlthr.levee.source._
import com.vlthr.levee.core.error._
import com.vlthr.levee.filters._
import com.vlthr.levee.util.Util
import scala.util.{Success, Failure, Try}
import validation.Result
import shapeless._

object LeveeParser {

  /** Creates a ParseContext from the given ANTLR context */
  def makeContext(c: ParserRuleContext, template: SourceFile) = {
    val stop = if (c.stop != null) c.stop else c.start
    val sourcePosition = SourcePosition.at(template,
                                           c.start.getStartIndex(),
                                           stop.getStopIndex() + 1)
    ParseContext(sourcePosition)
  }

  def getParseTree(node: String): String = {
    val (parser, errors) = makeParser(SourceFile.fromFile(InMemoryFile(node)))
    val tree = parser.template()
    tree.toStringTree(parser)
  }

  /** Parse a given template to an AST */
  def parse(file: SourceFile)(implicit ctx: Context): Validated[Obj] = {
    val (parser, errors) = makeParser(file)
    val tree = parser.template()
    val result = tree.accept(new LiquidNodeVisitor(file))
    if (errors.errors.size != 0) {
      val (firstError, rest) = errors.errors.toList.splitAt(1)
      Result.invalids(
        validation.NonEmptyVector(firstError.head, rest.toVector))
    } else Result.valid(result)
  }

  /** Parse a given template path to an AST */
  def parse(path: String)(implicit ctx: Context): Validated[Obj] =
    parse(SourceFile.fromFile(TemplateFile(path)))

  def makeParser(file: SourceFile): (LiquidParser, GatherErrors) = {
    val input = new ANTLRInputStream(file.content, file.content.size)
    val lexer = new LiquidLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new LiquidParser(tokens)
    val errors = new GatherErrors(file)
    lexer.removeErrorListeners();
    lexer.addErrorListener(errors);
    parser.removeErrorListeners();
    parser.addErrorListener(errors);
    (parser, errors)
  }
}

/** Error listener that gathers all ANTLR errors and wraps them in Levee Errors. */
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

/** To convert the ANTLR parse tree to an AST, we create visitors for the parse tree that
  * extract the relevant information and construct the AST nodes. */
class LiquidNodeVisitor(template: SourceFile)(implicit val ctx: Context)
    extends LiquidParserBaseVisitor[Obj] {

  override def visitNode(c: LiquidParser.NodeContext): Obj = {
    if (c.tag != null) {
      visitTag(c.tag)
    } else if (c.output != null) {
      visitOutput(c.output)
    } else if (c.TEXT != null) {
      visitText(c.TEXT)
    } else {
      throw new Exception("Missing node definition");
    }
  }

  def visitText(t: TerminalNode): Obj = {
    val sourcePosition = SourcePosition.at(template,
                                           t.getSymbol().getStartIndex(),
                                           t.getSymbol().getStopIndex() + 1)
    implicit val pc = ParseContext(sourcePosition)
    TextNode(t.getText())
  }

  override def visitOutput(c: LiquidParser.OutputContext): Obj = {
    implicit val pc = LeveeParser.makeContext(c, template)
    val output_expr = c.output_expr()
    OutputNode(visitOutputExpr(output_expr))
  }

  def visitOutputExpr(c: LiquidParser.Output_exprContext): Expr = {
    implicit val pc = LeveeParser.makeContext(c, template)
    val expVisitor = new LiquidExprVisitor(template)
    if (c.FILTER() != null) {
      val args =
        if (c.COLON() != null)
          new LiquidArgsVisitor(template).visitArglist(c.arglist())
        else Nil
      val kwargs =
        if (c.COLON() != null)
          new LiquidKwArgsVisitor(template).visitKwargs(c.kwargs())
        else Map[String, Expr]()
      val filter = ctx.getFilter(c.id.getText)
      FilterExpr(visitOutputExpr(c.output_expr()), filter, args, kwargs)
    } else {
      expVisitor.visitExpr(c.expr())
    }
  }

  override def visitTag(c: LiquidParser.TagContext): Obj = {
    implicit val pc = LeveeParser.makeContext(c, template)
    if (c.ifTag() != null) {
      val ev = new LiquidExprVisitor(template)
      val expr =
        ev.visitExpr(c.ifTag().ifStart().expr())
      val block = visitBlock(c.ifTag().block())
      val elsifs =
        if (c.ifTag().elsif() != null)
          c.ifTag()
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
    } else if (c.caseTag != null) {
      val ev = new LiquidExprVisitor(template)
      val expr =
        ev.visitExpr(c.caseTag.expr)
      val whens =
        if (c.caseTag.whenBlock != null)
          c.caseTag.whenBlock.asScala
            .map(when => (ev.visitExpr(when.expr), visitBlock(when.block)))
            .toList
        else Nil
      val els =
        if (c.caseTag.els != null)
          Some(visitBlock(c.caseTag.els.block))
        else None
      CaseTag(expr, whens, els)
    } else if (c.forTag() != null) {
      val id = c.forTag().forStart().id().getText()
      val expr = visitOutputExpr(c.forTag().forStart().output_expr())
      val block = visitBlock(c.forTag.block)
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
    } else if (c.breakTag != null) {
      BreakTag()
    } else if (c.continueTag != null) {
      ContinueTag()
    } else if (c.customTag != null) {
      val id = c.customTag.id.getText
      val args =
        if (c.customTag.arglist != null)
          new LiquidArgsVisitor(template).visitArglist(c.customTag.arglist)
        else Nil
      val tag = ctx.getTag(id)
      CustomTag(tag, args)
    } else {
      throw new Exception(MalformedTag().getMessage) // TODO: Specialize
    }
  }

  override def visitTemplate(c: LiquidParser.TemplateContext): Obj = {
    visitBlock(c.block())
  }

  override def visitBlock(c: LiquidParser.BlockContext): Obj = {
    implicit val pc = LeveeParser.makeContext(c, template)
    BlockNode(c.node().asScala.toList.map(n => visitNode(n)))
  }
}

/** Visitor for constructing argument lists */
class LiquidArgsVisitor(template: SourceFile)(implicit val ctx: Context)
    extends LiquidParserBaseVisitor[List[Expr]] {
  override def visitArglist(c: LiquidParser.ArglistContext): List[Expr] = {
    if (c == null) Nil
    c.expr()
      .asScala
      .map(ec => {
        implicit val pc = LeveeParser.makeContext(c, template)
        new LiquidExprVisitor(template).visitExpr(ec)
      })
      .toList
  }
}

/** Visitor for constructing keyword arguments into a map */
class LiquidKwArgsVisitor(template: SourceFile)(implicit val ctx: Context)
    extends LiquidParserBaseVisitor[Map[String, Expr]] {
  override def visitKwargs(c: LiquidParser.KwargsContext): Map[String, Expr] =
    if (c == null) Map()
    else
      Map(
        c.kwarg.asScala
          .map(ec => {
            implicit val pc = LeveeParser.makeContext(c, template)
            val id = ec.id.getText
            val expr = new LiquidExprVisitor(template).visitExpr(ec.expr)
            (id, expr)
          }): _*)
}

class LiquidExprVisitor(template: SourceFile)(implicit val ctx: Context)
    extends LiquidParserBaseVisitor[Expr] {
  override def visitExpr(c: LiquidParser.ExprContext): Expr = {
    implicit val pc = LeveeParser.makeContext(c, template)
    if (c.DOTINDEX() != null) {
      DotExpr(visitExpr(c.expr(0)), c.id().getText())
    } else if (c.STARTINDEX() != null) {
      IndexExpr(visitExpr(c.expr(0)), visitExpr(c.expr(1)))
    } else if (c.LPAR != null && c.RPAR != null && c.RANGE() == null) {
      visitExpr(c.expr(0)) // ParenExpr
    } else if (c.RANGE() != null) {
      RangeExpr(visitExpr(c.expr(0)), visitExpr(c.expr(1)))
    } else if (c.CONTAINS() != null) {
      ContainsExpr(visitExpr(c.expr(0)), visitExpr(c.expr(1)))
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
    implicit val pc = LeveeParser.makeContext(c, template)
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
