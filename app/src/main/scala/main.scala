package vlthr.tee
import vlthr.tee.parser._
import scala.collection.JavaConverters._
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

object GenTestExamples extends App {
  args.foreach { input =>
    val basename = input.replaceAll("\\.liquid", "")
    val template = io.Source.fromFile(input).mkString
    val ast = Liquid.parse(template)
    val parseTree = Liquid.getParseTree(template)
    Files.write(Paths.get(basename + ".ast"),
                ast.toString.getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get(basename + ".parseTree"),
                parseTree.toString.getBytes(StandardCharsets.UTF_8))
  }
}
