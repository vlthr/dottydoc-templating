package vlthr.tee.parser

import scala.io.Source
import scala.util.Try
import org.junit._
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters
import vlthr.tee.core._
import scala.collection.JavaConverters._
import java.nio.file.{FileSystems, Path, Paths, Files}
import java.nio.charset.StandardCharsets

/** For every .liquid file in the examples directory, compare the outputs
  * of various stages of template rendering to their expected values.
  */
@RunWith(classOf[Parameterized])
class FileTests(template: Path) {
  var templateBody: String = null
  var result: Try[Node] = null

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
    Assume.assumeTrue(result.isSuccess);
    fileTest(".render") { templateBody =>
      implicit val ctx = EvalContext()
      Liquid.parse(templateBody).get.render()
    }
  }

  @Test def testAST() = {
    Assume.assumeTrue(result.isSuccess);
    fileTest(".ast") { templateBody =>
      Liquid.parse(templateBody).toString
    }
  }

  /** Asserts that the output of f(template_string) matches the expected
    * value found in filename.ext-expected. Saves the actual output to filename.ext
    */
  def fileTest(ext: String)(f: Function[String, String]) = {
    val outFile = FileTests.pairedFileWithExt(template, ext)
    val expectedFile =
      FileTests.pairedFileWithExt(template, ext + "-expected")
    Assume.assumeTrue(Files.exists(expectedFile))
    val actual = f(templateBody)
    FileTests.writeFile(outFile, actual)
    val expected = FileTests.readWholeFile(expectedFile)
    assertEquals(expected, actual)
  }

}

object FileTests {
  @Parameters(name="{0}")
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
