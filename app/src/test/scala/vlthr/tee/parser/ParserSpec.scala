package vlthr.tee.parser

import scala.io.Source
import scala.util.Try
import org.scalatest._
import vlthr.tee.core._

object Incomplete extends org.scalatest.Tag("Incomplete")
class ParserSpec extends FlatSpec with Matchers {
  behavior of "Parser"
  it should "parse whole templates" in {
    val input = """
    # Heading
    ## Subheading about {{ topic }}
    {% if true %}
        {{ content.body }}
    {% endif %}
    """
    // val node = Liq.parseTemplate(input)
    // node match {
    //   case BlockNode(_) =>
    //   case _ => {
    //     fail("" + node)
    //   }
    // }
  }
  it should "parse assign tags" in {
    val assignStr = "{% assign a = 1 %}"
    val assignNode = Liq.parseNode(assignStr)
    assignNode match {
      case AssignTag(_, _) =>
      case _ => {
        fail("" + assignNode)
      }
    }
  }
  it should "parse if blocks" in {
    val ifStr = """
    {% if true %}
    {{ true }}
    {{ true }}
    {% endif %}
    """
    val ifNode = Liq.parseNode(ifStr)
    ifNode match {
      case IfTag(_, _) =>
      case _ => {
        fail("" + ifNode)
      }
    }
  }
  it should "parse for blocks" in {
    val forStr = """
    {% for a in "1,2,3,4"; | split: ","; %}
    {{ true }}
    {{ true }}
    {% endfor %}
    """
    val forNode = Liq.parseNode(forStr)
    forNode match {
      case ForTag(_, _, _) =>
      case _ => {
        fail("" + forNode)
      }
    }
  }
  it should "parse ids" in {
    val id = Liq.parseExpr("Identifier")
    id match {
      case VariableUseExpr(_) =>
      case _ => {
        fail("" + id)
      }
    }
  }
  it should "parse literals" in {
    val int = Liq.parseExpr("1")
    int match {
      case LiteralExpr(IntValue(1)) =>
      case _ => {
        fail("" + int)
      }
    }
    val sstr = Liq.parseExpr("'single quote string'")
    sstr match {
      case LiteralExpr(StringValue("single quote string")) =>
      case _ => {
        fail("" + sstr)
      }
    }

    val dstr = Liq.parseExpr("\"double quote string\"")
    dstr match {
      case LiteralExpr(StringValue("double quote string")) =>
      case _ => {
        fail("" + dstr)
      }
    }

    val bool = Liq.parseExpr("false")
    bool match {
      case LiteralExpr(BooleanValue(false)) =>
      case _ => {
        fail("" + bool)
      }
    }
  }
  it should "parse nodes" in {
    ("{{1}}" :: "{{''}}" :: "{{ 1}}" :: "{{    true  }}" :: Nil)
      .foreach(s => {
        val output = Liq.parseNode(s)
        output match {
          case OutputNode(_) => println(output)
          case _ =>
            fail(
              "String " + s + " parsed to an invalid output node: " + output)
        }
      })
  }
  it should "track the source position of each node" in {
    val output = Liq.parseNode("{{  'str'}}")
    output match {
      case o @ OutputNode(e) => {
        o.parseContext.begin should be(0);
        o.parseContext.end should be(10);
        e.parseContext.begin should be(4);
        e.parseContext.end should be(8);
      }
      case _ => fail("" + output)
    }
  }
  ignore should "work on every tag in dottydoc" taggedAs (Incomplete) in {
    val source = Source.fromURL(getClass.getResource("/tags.txt"))
    val (successes, failures) =
      source.getLines.map(l => Try(Liq.parseNode(l))).partition(_.isSuccess)
    println(failures)
    failures.size should be(0)
  }
  it should "parse indexing" in {
    val index = Liq.parseExpr("a[b][c]")
    println(index)
    index match {
      case IndexExpr(IndexExpr(_, _), VariableUseExpr(_)) =>
      case _ => fail("String output node: " + index)
    }
  }
  it should "parse field indexing" in {
    val dotindex = Liq.parseExpr("a.b.c")
    println(dotindex)
    dotindex match {
      case DotExpr(DotExpr(_, _), StringValue(_)) =>
      case _ => fail("String output node: " + dotindex)
    }
  }
  it should "parse filter applications" in {
    val output = Liq.parseNode("{{ 'str' | reverse }}")
    output match {
      case OutputNode(FilterExpr(_, _, Nil)) => println(output)
      case _ => fail("String output node: " + output)
    }
    val out = Liq.parseNode("{{ 'str' | filter: 'a', 't' }}")
    out match {
      case OutputNode(FilterExpr(_, _, args)) => args.size should be(2)
      case _ => fail("String output node: " + out)
    }
  }
}
