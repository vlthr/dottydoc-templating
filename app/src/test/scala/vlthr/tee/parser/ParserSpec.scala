package vlthr.tee.parser

import org.scalatest._
import vlthr.tee.core._

class ParserSpec extends FlatSpec with Matchers {
  behavior of "Parser"
  it should "parse literals" in {
    val int = Liquid.parseExpr("1")
    int match {
      case OutputNode(LiteralExpr(IntValue(1))) =>
      case _ => {
        fail(""+int)
      }
    }
    val sstr = Liquid.parseExpr("'single quote string'")
    sstr match {
      case OutputNode(LiteralExpr(StringValue("single quote string"))) =>
      case _ => {
        fail(""+sstr)
      }
    }

    val dstr = Liquid.parseExpr("\"double quote string\"")
    dstr match {
      case OutputNode(LiteralExpr(StringValue("double quote string"))) =>
      case _ => {
        fail(""+dstr)
      }
    }
  }
}
