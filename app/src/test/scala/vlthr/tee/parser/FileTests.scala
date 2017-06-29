package vlthr.tee.parser

import scala.io.Source
import scala.util.{Success, Failure, Try}
import org.junit._
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters
import vlthr.tee.core._
import scala.collection.JavaConverters._
import java.nio.file.{FileSystems, Path, Paths, Files}
import java.nio.charset.StandardCharsets
import scala.collection.mutable.Map
import liqp.Template

/** For every .liquid file in the examples directory, compare the outputs
  * of various stages of template rendering to their expected values.
  */
@RunWith(classOf[Parameterized])
class FileTests(template: Path) {
  var templateBody: String = null
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

  @Before def setup() = {
    templateBody = FileTests.readWholeFile(template)
    result = Liquid.parse(templateBody)
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
      implicit val ctx: EvalContext = EvalContext.createNew(environment.map {
        case (k, v) => (k, Value.create(v))
      })
      result.get.render match {
        case Success(output) => output
        case Failure(f) => f.getMessage
      }
    }
  }

  @Test def testMatchesLiqp(): Unit = {
    Assume.assumeTrue(result.isSuccess)
    val subList = 1 :: Nil
    val listOfLists = subList :: Nil
    implicit val ctx: EvalContext = EvalContext.createNew(environment.map {
      case (k, v) => (k, Value.create(v))
    })
    val actual = result.get.render
    if (actual.isFailure) return ()

    val template = Template.parse(templateBody)
    val expected = template.render(asJava(environment))
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
    val outFile = FileTests.pairedFileWithExt(template, ext)
    val expectedFile =
      FileTests.pairedFileWithExt(template, ext + "-expected")
    val actual = f(templateBody)
    FileTests.writeFile(outFile, actual)
    Assume.assumeTrue(Files.exists(expectedFile))
    val expected = FileTests.readWholeFile(expectedFile)
    assertEquals(expected, actual)
  }
}

object FileTests {
  @Parameters(name = "{0}")
  def data(): java.util.Collection[Array[Object]] = {
    filesInDir("./app/src/test/resources/examples")
      .filter(_.toString.endsWith(".liquid"))
      .map { file =>
        List(file.asInstanceOf[Object]).toArray
      }
      .toList
      .asJava
  }

  def filesInDir(dir: String): Iterator[Path] = {
    val path = FileSystems.getDefault.getPath(dir)
    Files.walk(path).iterator().asScala.filter(Files.isRegularFile(_))
  }
  def pairedFileWithExt(f: Path, ext: String): Path = {
    val basename = f.toString.replaceAll("\\.liquid", "")
    Paths.get(basename + ext)
  }
  def readWholeFile(path: Path): String =
    Source.fromFile(path.toString).mkString
  def writeFile(path: Path, contents: String) =
    Files.write(path, contents.getBytes(StandardCharsets.UTF_8))
}
