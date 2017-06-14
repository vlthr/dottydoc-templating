package vlthr.tee.parser

import org.scalatest._
import vlthr.tee.core._

class ParserSpec extends FlatSpec with Matchers {
  behavior of "Parser"
  it should "parse literals" in {
    val result = Liquid.parseExpr("1")
    result match {
      case OutputNode(LiteralExpr(IntValue(1))) =>
      case _ => {
        fail(""+result)
      }
    }

  }
}
