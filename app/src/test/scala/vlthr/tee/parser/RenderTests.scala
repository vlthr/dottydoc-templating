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

  @Test def testHListFilter() = {
    import shapeless._
    import vlthr.tee.filters._
    import vlthr.tee.core.Value._
    val f1 = Filter[IntValue, StringValue :: HNil, Empty]("f1") {
      (ctx, filter, input, args, optArgs) =>
        val pattern = args.head.get
        val n = input.get
        Result.valid(StringValue(s"$n, $pattern"))
    }
    f1(IntValue(1), StringValue("a") :: Nil) match {
      case Valid(output) => assertEquals(StringValue("1, a"), output)
      case Invalid(f) => fail("Filter could not render.")
      case Invalids(f) => fail("Filter could not render.")
    }
    ()
  }
}
