package vlthr.tee.core
import vlthr.tee.filters._
import vlthr.tee.core.Error._
import vlthr.tee.core._
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scala.util.{Try, Success, Failure}

case class SourceFile(body: String, path: String) {
  final val LF = '\u000A'
  final val FF = '\u000C'
  final val CR = '\u000D'
  final val SU = '\u001A'

  /** Is character a line break? */
  def isLineBreakChar(c: Char) = c match {
    case LF|FF|CR|SU  => true
    case _            => false
  }

  private def isLineBreak(idx: Int) =
    if (idx >= length) false else {
      val ch = body(idx)
      // don't identify the CR in CR LF as a line break, since LF will do.
      if (ch == CR) (idx + 1 == length) || (body(idx + 1) != LF)
      else isLineBreakChar(ch)
    }

  private def calculateLineIndices(cs: Array[Char]) = {
    val buf = new ArrayBuffer[Int]
    buf += 0
    for (i <- 0 until cs.length) if (isLineBreak(i)) buf += i + 1
    buf += cs.length // sentinel, so that findLine below works smoother
    buf.toArray
  }
  private lazy val lineIndices: Array[Int] = calculateLineIndices(body.toArray)

  /** Map line to offset of first character in line */
  def lineToOffset(index: Int): Int = lineIndices(index)

  val length = body.length
}

case class SourcePosition(start: Int, end: Int, template: SourceFile) {
  def display: String = template.body.substring(start, end+1)
  def report: String = linesOfContext(1)
  def linesOfContext(count: Int): String = {
    def seekNewline(str: String, start: Int, direction: Int, count: Int): Int = {
      var c = start
      var remaining = count
      while (remaining > 0 && ((direction < 0 && c > 0 && c < str.size) || (direction > 0 && c < (str.size - 1) && c >= 0))) {
        if (str(c) == '\n') {
          remaining -= 1
        }
        c += direction
      }
      c
    }
    val s = seekNewline(template.body, start, -1, count + 1)
    val e = seekNewline(template.body, start, 1, count + 1)
    template.body.substring(s, e)
  }
}

object SourcePosition {
  def fromLine(sourceFile: SourceFile, line: Int, charPositionInLine: Int, length: Int) = {
    val start = sourceFile.lineToOffset(line) + charPositionInLine
    val stop = start + length
    SourcePosition(start, stop, sourceFile)
  }
}

trait Renderable {
  def render()(implicit evalContext: EvalContext): Try[String]
}

abstract trait Node extends Renderable {
  def sourcePosition: SourcePosition
}

abstract trait Expr extends Renderable {
  def sourcePosition: SourcePosition
  def eval()(implicit evalContext: EvalContext): Try[Value] = ???
  def render()(implicit evalContext: EvalContext): Try[String] =
    eval.flatMap(_.render)
}

case class EvalContext(mappings: MMap[String, Value],
                       parent: Option[EvalContext],
                       includeDir: String) {
  def lookup(s: String): Option[Value] =
    mappings.get(s).orElse(parent.flatMap(_.lookup(s)))

}

object EvalContext {
  def createNew(): EvalContext = createNew(Map(), "")
  def createNew(map: Map[String, Value], includeDir: String): EvalContext =
    EvalContext(MMap(map.toSeq: _*), None, includeDir)
  def createChild(parent: EvalContext): EvalContext =
    EvalContext(MMap(), Some(parent), parent.includeDir)
}

abstract trait Filter {
  def name: String
  def apply(input: Value, args: List[Value])(
      implicit evalContext: EvalContext, parent: FilterExpr): Try[Value]
  def isDefinedForInput(v: Value): Boolean
  def isDefinedForArgs(v: List[Value]): Boolean
  def typeCheck(input: Value, args: List[Value])(implicit parent: FilterExpr): Try[Unit] = {
    val inputErrors =
      if (!isDefinedForInput(input))
        List(InvalidFilterInput(parent, this, input))
      else Nil
    val argsErrors =
      if (!isDefinedForArgs(args))
        List(InvalidFilterArgs(parent, this, args))
      else Nil
    val errors = inputErrors ++ argsErrors
    if (errors.size > 0) Error.fail(errors: _*)
    else Success(())
  }
}

sealed trait Truthable {
  def truthy: Boolean
}

trait Truthy extends Truthable {
  def truthy = true
}

sealed trait Value extends Renderable with Truthable with Ordered[Value] {
  def display: String

  def typeName: String

  def compare(that: Value): Int = {
    (this, that) match {
      case (IntValue(l), IntValue(r)) => l compare r
      case (StringValue(l), StringValue(r)) => l compare r
      case (BooleanValue(l), BooleanValue(r)) => l compare r
      case (MapValue(l), MapValue(r)) => ???
      case (ListValue(l), ListValue(r)) => ???
      case (l, r) => throw new Exception(s"TODO: Incomparable types $l and $r")
    }
  }
}

sealed trait IndexedValue extends Value

final case class StringValue(v: String) extends Value with Truthy {
  def display: String = s""""$v""""
  def typeName: String = "String"
  def render()(implicit evalContext: EvalContext): Try[String] = Success(v)
}

final case class BooleanValue(v: Boolean) extends Value {
  def display: String = s"""$v"""
  def typeName: String = "Boolean"
  def render()(implicit evalContext: EvalContext): Try[String] =
    Success(v.toString)
  def truthy = v
}

final case class IntValue(v: Int) extends Value with Truthy {
  def display: String = s"""$v"""
  def typeName: String = "Integer"
  def render()(implicit evalContext: EvalContext): Try[String] =
    Success(v.toString)
}

final case class MapValue(v: Map[String, Value])
    extends IndexedValue
    with Truthy {
  def display: String = ???
  def typeName: String = "Map"
  def render()(implicit evalContext: EvalContext): Try[String] = throw UnrenderableValueException()
}

final case class ListValue(v: List[Value]) extends IndexedValue with Truthy {
  def display: String = s"""[${v.map(_.display).mkString(", ")}]"""
  def typeName: String = "Array"
  def render()(implicit evalContext: EvalContext): Try[String] = throw UnrenderableValueException()
}

object Value {
  def create(value: Any): Value = {
    value match {
      case v: Value => v
      case v: Int => IntValue(v)
      case v: String => StringValue(v)
      case v: Boolean => BooleanValue(v)
      case v: Char => StringValue(""+v)
      case v: Map[String, _] =>
        MapValue(v.map { case (key, value) => (key, Value.create(value)) })
      case v: Seq[_] => ListValue(v.map(value => Value.create(value)).toList)
      case _ => throw new Exception(s"Invalid value: $value")
    }
  }
}
