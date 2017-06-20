package vlthr.tee
import vlthr.tee.parser._
import scala.collection.JavaConverters._
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

object GenTestExamples extends App {
  args.foreach { input =>
    val basename = input.replaceAll("\\.liquid", "")
    val ast = Liquid.parse(io.Source.fromFile(input).mkString)
    Files.write(Paths.get(basename + ".ast"),
                ast.toString.getBytes(StandardCharsets.UTF_8))
  }
}
