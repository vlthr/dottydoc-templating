package vlthr.tee.parser

import org.scalatest._
import vlthr.tee.core._

class ParserSpec extends FlatSpec with Matchers {
  behavior of "Parser"
  it should "parse literals" in {
    val int = Liquid.parseExpr("1")
    int match {
      case LiteralExpr(IntValue(1)) =>
      case _ => {
        fail(""+int)
      }
    }
    val sstr = Liquid.parseExpr("'single quote string'")
    sstr match {
      case LiteralExpr(StringValue("single quote string")) =>
      case _ => {
        fail(""+sstr)
      }
    }

    val dstr = Liquid.parseExpr("\"double quote string\"")
    dstr match {
      case LiteralExpr(StringValue("double quote string")) =>
      case _ => {
        fail(""+dstr)
      }
    }

    val bool = Liquid.parseExpr("false")
    bool match {
      case LiteralExpr(BooleanValue(false)) =>
      case _ => {
        fail(""+bool)
      }
    }
  }
  it should "parse nodes" in {
    ("{{1}}" :: "{{''}}" :: "{{ 1}}" :: "{{    true  }}" :: Nil)
      .foreach(s => {
                 val output = Liquid.parseNode(s)
                 output match {
                   case OutputNode(_) => println(output)
                   case _ => fail("String "+s+" parsed to an invalid output node: "+output)
                 }
               })
  }
  it should "track the source position of each node" in {
    val output = Liquid.parseNode("{{  'str'}}")
    output match {
      case o@OutputNode(e) => {
        o.parseContext.begin should be(4);
        o.parseContext.end should be(8);
        e.parseContext.begin should be(4);
        e.parseContext.end should be(8);
      }
      case _ => fail(""+output)
    }
  }
}
