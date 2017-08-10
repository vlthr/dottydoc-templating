package vlthr.tee.parser

import scala.io.Source
import scala.util.{Success, Failure, Try}
import org.junit._
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters
import vlthr.tee.core._
import vlthr.tee.util._
import vlthr.tee.typetraits.TypeTraits._
import scala.collection.JavaConverters._
import java.nio.file.{FileSystems, Path, Paths, Files}
import java.nio.charset.StandardCharsets
import scala.collection.mutable.{Map => MMap}
import validation.Result
import validation.Result._
import liqp.Template
import liqp.tags.Include

class RenderTests {
  val includeDir = "./examples/_includes"
  val environment = Map(
    "zero" -> 0,
    "map" -> Map("id" -> 1, "subMap" -> Map("id" -> 1)),
    "content" -> "CONTENT",
    "site" -> Map("baseurl" -> "BASEURL"),
    "page" -> Map("title" -> "TITLE",
                  "extraCSS" -> List("extraCSS1", "extraCSS2", "extraCSS3"),
                  "extraJS" -> List("extraJS1", "extraJS2", "extraJS3")),
    "listOfLists" -> List(List(1)),
    "list" -> List(1)
  )
  implicit val ctx: Context = Context
    .createNew()
    .withParams(environment.map {
      case (k: String, v: Object) => (k, Value.create(v))
    })
    .withIncludeDir(includeDir)

  // @Test def testCustomFilter() = {
  //   case class CustomFilter()
  //       extends Filter
  //       with InputType(ValueType.Integer)
  //       with FixedArgs(ValueType.Integer)
  //       with FixedOptArgs(ValueType.Integer)
  //       with OptKwArgs("limit" -> ValueType.Integer) {
  //     def name = "customFilter"
  //     def filter(input: Value, args: List[Value], kwargs: Map[String, Value])(
  //         implicit ctx: Context): Validated[Value] = {
  //       val a = args(0).asInstanceOf[IntValue].v
  //       val b = args(1).asInstanceOf[IntValue].v
  //       val c = kwargs("c").asInstanceOf[IntValue].v
  //       Valid(IntValue(a + b + c))
  //     }
  //   }
  //   val body = """{{ 1 | customFilter: 1, 1 c: 1 }}"""
  //   implicit val newCtx: Context = ctx.withFilter(CustomFilter())
  //   Liquid.renderString(body, environment, includeDir, ctx = Some(newCtx)) match {
  //     case Result.valid(output) => assertEquals("3", output)
  //     case Failure(f) => fail("Custom filter could not render.")
  //   }
  //   ()
  // }

  @Test def testHListFilter() = {
    import vlthr.tee.filters._
    import shapeless._
    // case class F1KwArgs(x: Option[IntValue])
    // val f1 = Filter[StringValue, StringValue :: HNil, Empty]("f1") { (ctx, filter, input, args, optArgs) =>
    //   val pattern = args.head.get
    //   val stringToSplit = input.get
    //   Result.valid(Value.create(stringToSplit.split(pattern).toList))
    // }
    // f1(IntValue(1), IntValue(1) :: StringValue("a") :: Nil) match {
    //   case Valid(output) => assertEquals(IntValue(1), output)
    //   case Invalid(f) => fail("Filter could not render.")
    //   case Invalids(f) => fail("Filter could not render.")
    // }
    ()
  }
}
