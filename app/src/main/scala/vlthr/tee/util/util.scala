package vlthr.tee.util

import scala.io.Source
import scala.util.{Success, Failure, Try}
import scala.collection.JavaConverters._
import java.nio.file.{FileSystems, Path, Paths, Files}
import java.nio.charset.StandardCharsets
import scala.collection.mutable.Map

object Util {
  def filesInDir(dir: String): Iterator[Path] = {
    val path = FileSystems.getDefault.getPath(dir)
    Files.walk(path).iterator().asScala.filter(Files.isRegularFile(_))
  }
  def pairedFileWithExt(f: String, ext: String): Path =
    pairedFileWithExt(Paths.get(f), ext)
  def pairedFileWithExt(f: Path, ext: String): Path = {
    val basename = f.toString.replaceAll("\\.liquid", "")
    Paths.get(basename + ext)
  }
  def readWholeFile(path: Path): String =
    Source.fromFile(path.toString).mkString
  def readWholeFile(path: String): String =
    Source.fromFile(path).mkString
  def writeFile(path: Path, contents: String) =
    Files.write(path, contents.getBytes(StandardCharsets.UTF_8))
}
