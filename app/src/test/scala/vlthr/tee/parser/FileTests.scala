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
import scala.collection.JavaConverters._
import java.nio.file.{FileSystems, Path, Paths, Files}
import java.nio.charset.StandardCharsets
import scala.collection.mutable.Map
import liqp.Template
import liqp.tags.Include

/** For every .liquid file in the examples directory, compare the outputs
  * of various stages of template rendering to their expected values.
  */
@RunWith(classOf[Parameterized])
class FileTests(file: SourceFile) {
  var result: Try[Node] = null
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
  implicit val ctx: EvalContext = EvalContext.createNew(environment.map {
    case (k: String, v: Object) => (k, Value.create(v))
  }, "./app/src/test/resources/examples/_includes")

  @Before def setup() = {
    result = Liquid.parse(file)
  }

  @Test def testParseTree(): Unit = {
    fileTest(".parseTree") { templateBody =>
      Liquid.getParseTree(templateBody)
    }
  }

  @Test def noErrors() = {
    assertTrue(result.isSuccess)
  }

  @Test def testRender() = {
    Assume.assumeTrue(result.isSuccess)
    fileTest(".render") { templateBody =>
      result.get.render match {
        case Success(output) => output
        case Failure(f) => f.getMessage
      }
    }
  }

  @Test def testMatchesLiqp(): Unit = {
    Assume.assumeTrue(result.isSuccess)
    val actual = result.get.render
    if (actual.isFailure) return ()

    val template = Template.parse(file.body)
    val expected = template.render(asJava(environment ++ Map(
      Include.INCLUDES_DIRECTORY_KEY -> "./app/src/test/resources/example/_includes")))
    assertEquals(expected, actual.get)
  }

  @Test def testAST() = {
    Assume.assumeTrue(result.isSuccess)
    fileTest(".ast") { templateBody =>
      result.get.toString
    }
  }

  def asJava(env: Map[String, Any]): java.util.Map[String, Object] = {
    def convert(scala: Any): Object = {
      scala match {
        case m: Map[String, Any] => asJava(m)
        case l: List[Any] => l.map(convert).asJava
        case v => v.asInstanceOf[Object]
      }
    }
    env
      .map { case (k, v) => (k, convert(v)) }
      .asJava
      .asInstanceOf[java.util.Map[String, Object]]
  }

  /** Asserts that the output of f(template_string) matches the expected
    * value found in filename.ext-expected. Saves the actual output to filename.ext
    */
  def fileTest(ext: String)(f: Function[String, String]) = {
    val outFile = Util.pairedFileWithExt(file.path, ext)
    val expectedFile =
      Util.pairedFileWithExt(file.path, ext + "-expected")
    val actual = f(file.body)
    Util.writeFile(outFile, actual)
    Assume.assumeTrue(Files.exists(expectedFile))
    val expected = Util.readWholeFile(expectedFile)
    assertEquals(expected, actual)
  }
}

object FileTests {
  @Parameters(name = "{0}")
  def data(): java.util.Collection[Array[Object]] = {
    Util
      .filesInDir("./app/src/test/resources/examples")
      .filter(_.toString.endsWith(".liquid"))
      .filter(!_.toString.contains("_include"))
      .map { file =>
        val sourceFile = SourceFile(Util.readWholeFile(file), file.toString)
        List(sourceFile.asInstanceOf[Object]).toArray
      }
      .toList
      .asJava
  }
}
