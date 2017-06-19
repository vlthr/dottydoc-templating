package vlthr.tee.parser

import scala.io.Source
import scala.util.Try
import org.scalatest._
import vlthr.tee.core._

object Incomplete extends org.scalatest.Tag("Incomplete")
class ParserSpec extends FlatSpec with Matchers {
  behavior of "Parser"
  it should "parse assign tags" in {
    val assignStr = "{% assign a = 1 %}"
    val assignNode = Liquid.parseNode(assignStr)
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
    val ifNode = Liquid.parseNode(ifStr)
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
    val forNode = Liquid.parseNode(forStr)
    forNode match {
      case ForTag(_, _, _) =>
      case _ => {
        fail("" + forNode)
      }
    }
  }
  it should "parse ids" in {
    val id = Liquid.parseExpr("Identifier")
    id match {
      case VariableUseExpr(_) =>
      case _ => {
        fail("" + id)
      }
    }
  }
  it should "parse literals" in {
    val int = Liquid.parseExpr("1")
    int match {
      case LiteralExpr(IntValue(1)) =>
      case _ => {
        fail("" + int)
      }
    }
    val sstr = Liquid.parseExpr("'single quote string'")
    sstr match {
      case LiteralExpr(StringValue("single quote string")) =>
      case _ => {
        fail("" + sstr)
      }
    }

    val dstr = Liquid.parseExpr("\"double quote string\"")
    dstr match {
      case LiteralExpr(StringValue("double quote string")) =>
      case _ => {
        fail("" + dstr)
      }
    }

    val bool = Liquid.parseExpr("false")
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
        val output = Liquid.parseNode(s)
        output match {
          case OutputNode(_) => println(output)
          case _ =>
            fail(
              "String " + s + " parsed to an invalid output node: " + output)
        }
      })
  }
  it should "track the source position of each node" in {
    val output = Liquid.parseNode("{{  'str'}}")
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
      source.getLines.map(l => Try(Liquid.parseNode(l))).partition(_.isSuccess)
    println(failures)
    failures.size should be(0)
  }
  it should "parse indexing" in {
    val index = Liquid.parseExpr("a[b][c]")
    println(index)
    index match {
      case IndexExpr(IndexExpr(_, _), VariableUseExpr(_)) =>
      case _ => fail("String output node: " + index)
    }
  }
  it should "parse field indexing" in {
    val dotindex = Liquid.parseExpr("a.b.c")
    println(dotindex)
    dotindex match {
      case DotExpr(DotExpr(_, _), StringValue(_)) =>
      case _ => fail("String output node: " + dotindex)
    }
  }
  it should "parse filter applications" in {
    val output = Liquid.parseNode("{{ 'str' | reverse }}")
    output match {
      case OutputNode(FilterExpr(_, _, Nil)) => println(output)
      case _ => fail("String output node: " + output)
    }
    val out = Liquid.parseNode("{{ 'str' | filter: 'a', 't' }}")
    out match {
      case OutputNode(FilterExpr(_, _, args)) => args.size should be(2)
      case _ => fail("String output node: " + out)
    }
  }
}
