package vlthr.tee.parser

import scala.io.Source
import scala.util.Try
import org.junit._
import org.junit.Assert._
import vlthr.tee.core._

class ParserTests {
  def assertParsed(str: String)(
      isMatch: PartialFunction[Node, Boolean]): Unit = {
    val ast = Liquid.parseTemplate(str)
    val error: PartialFunction[Node, Unit] = {
      case bad => {
        val parseTree = Liquid.getParseTree(str)
        fail(
          s"""Failed parse! String \"$str\" yielded unexpected result: $bad with (block mode) parse tree: $parseTree""")
      }
    }
    (isMatch orElse error)(ast)
  }
  @Test def parseTemplate() = {
    val input = """
    # Heading
    ## Subheading about {{ topic }}
    {% if true %}
        {{ content.body }}
    {% endif %}
    """
    assertParsed(input) {
      case BlockNode(_) => true
    }
  }
  @Test def parseAssign() = {
    val input = "{% assign a = 1 %}"
    assertParsed(input) {
      case BlockNode(AssignTag(_, _) :: rest) => true
    }
  }
  @Test def parseIf() = {
    val input = """{% if true %}
      {{ true }}
      {{ true }}
      {% endif %}"""
    assertParsed(input) {
      case BlockNode(IfTag(_, _) :: rest) => true
    }
  }
  @Test def parseFor() = {
    val input = """{% for a in "1,2,3,4"; | split: ","; %}
      {{ true }}
      {{ true }}
      {% endfor %}"""
    assertParsed(input) {
      case BlockNode(ForTag(_, _, _) :: rest) => true
    }
  }
  @Test def parseId() = {
    val input = "{{ Identifier }}"
    assertParsed(input) {
      case BlockNode(OutputNode(VariableUseExpr(_)) :: Nil) => true
    }
  }
  @Test def parseInt() = {
    val input = "{{ 1 }}"
    assertParsed(input) {
      case BlockNode(OutputNode(LiteralExpr(IntValue(1))) :: Nil) => true
    }
  }
  @Test def parseString() = {
    val input = "{{ 'single quote string' }}"
    assertParsed(input) {
      case BlockNode(OutputNode(LiteralExpr(StringValue(_))) :: Nil) => true
    }
    val input2 = "{{ 'single quote string' }}"
    assertParsed(input2) {
      case BlockNode(OutputNode(LiteralExpr(StringValue(_))) :: Nil) => true
    }
  }
  @Test def parseMisc() = {
    val inputs = ("{{1}}" :: "{{''}}" :: "{{ 1}}" :: "{{    true  }}" :: Nil)
    inputs.foreach(input => {
      assertParsed(input) {
        case BlockNode(OutputNode(LiteralExpr(_)) :: Nil) => true
      }
    })
  }

  @Test def shouldTrackSourcePosition() = {
    val input = "{{  'str'}}"
    assertParsed(input) {
      case BlockNode((o @ OutputNode(e)) :: Nil) => {
        assertEquals(0, o.parseContext.begin);
        assertEquals(10, o.parseContext.end);
        assertEquals(4, e.parseContext.begin);
        assertEquals(8, e.parseContext.end);
        true
      }
    }
  }
  @Ignore
  @Test def shouldWorkOnAllDottyDocTags() = {
    val source = Source.fromURL(getClass.getResource("/tags.txt"))
    val (successes, failures) =
      source.getLines.map(l => Try(Liquid.parseNode(l))).partition(_.isSuccess)
    println(failures)
    assertEquals(0, failures.size)
  }
  @Test def parseIndexing() = {
    val input = "{{ a[b][c] }}"
    assertParsed(input) {
      case BlockNode(
          OutputNode(IndexExpr(IndexExpr(_, _), VariableUseExpr(_))) :: Nil) =>
        true
    }
  }
  @Test def parseDotIndexing() = {
    val input = "{{ a.b.c }}"
    assertParsed(input) {
      case BlockNode(OutputNode(DotExpr(DotExpr(_, _), "c")) :: Nil) => true
    }
  }
  @Test def parseFilterApplication() = {
    val input = "{{ 'str' | reverse }}"
    assertParsed(input) {
      case BlockNode(OutputNode(FilterExpr(_, _, Nil)) :: Nil) => true
    }
  }
  @Ignore
  @Test def parseMultipleFilterApplication() = {
    val input = "{{ '1,2,3' | split: ',' | reverse }}"
    assertParsed(input) {
      case BlockNode(
          OutputNode(FilterExpr(FilterExpr(_, _, _), _, Nil)) :: Nil) =>
        true
    }
  }
}
