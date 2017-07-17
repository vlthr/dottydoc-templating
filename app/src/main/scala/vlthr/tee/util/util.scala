package vlthr.tee.util

import vlthr.tee.core._
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

  def convert(value: Value): Object = {
    value match {
      case MapValue(m) => m.map { case (k, v) => (k, convert(v)) }.asJava
      case ListValue(l) => l.map(convert).toList.asJava
      case BooleanValue(b) => b.asInstanceOf[Object]
      case IntValue(i) => i.asInstanceOf[Object]
      case StringValue(s) => s.asInstanceOf[Object]
    }
  }

  def asJava(value: MapValue): java.util.Map[String, Object] = {
    convert(value).asInstanceOf[java.util.Map[String, Object]]
  }

  def asJava(value: ListValue): java.util.List[Object] = {
    convert(value).asInstanceOf[java.util.List[Object]]
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
}
