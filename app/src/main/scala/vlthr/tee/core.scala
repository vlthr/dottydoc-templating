package vlthr.tee.core
import vlthr.tee.filters._
import scala.collection.mutable.Map
import scala.util.{Try, Success, Failure}

case class SourceFile(body: String, path: String)

case class SourcePosition(start: Int, end: Int, template: SourceFile) {
  def report: String = linesOfContext(1)
  def linesOfContext(count: Int): String = {
    def seekNewline(str: String, start: Int, direction: Int, count: Int): Int = {
      var c = start
      var remaining = count
      while (count > 0 && c > 0 && c < (str.size - 1)) {
        c += direction
        if (str(c) == '\n') {
          remaining -= 1
        }
      }
      c
    }
    val s = seekNewline(template.body, start, -1, count + 1)
    val e = seekNewline(template.body, start, 1, count + 1)
    val highlightStart = start - s
    val highlightEnd = end - e
    template.body.substring(s, e)
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

case class EvalContext(mappings: Map[String, Value],
                       parent: Option[EvalContext],
                       includeDir: String) {
  def lookup(s: String): Option[Value] =
    mappings.get(s).orElse(parent.flatMap(_.lookup(s)))

}

object EvalContext {
  def createNew(): EvalContext = createNew(Map(), "")
  def createNew(map: Map[String, Value], includeDir: String): EvalContext =
    EvalContext(map, None, includeDir)
  def createChild(parent: EvalContext): EvalContext =
    EvalContext(Map(), Some(parent), parent.includeDir)
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
  def render()(implicit evalContext: EvalContext): Try[String] = Success(v)
}

final case class BooleanValue(v: Boolean) extends Value {
  def render()(implicit evalContext: EvalContext): Try[String] =
    Success(v.toString)
  def truthy = v
}

final case class IntValue(v: Int) extends Value with Truthy {
  def render()(implicit evalContext: EvalContext): Try[String] =
    Success(v.toString)
}

final case class MapValue(v: Map[String, Value])
    extends IndexedValue
    with Truthy {
  def render()(implicit evalContext: EvalContext): Try[String] = ???
}

final case class ListValue(v: List[Value]) extends IndexedValue with Truthy {
  def render()(implicit evalContext: EvalContext): Try[String] = ???
}

object Value {
  def create(value: Any): Value = {
    value match {
      case v: Value => v
      case v: Int => IntValue(v)
      case v: String => StringValue(v)
      case v: Boolean => BooleanValue(v)
      case v: Map[String, Any] =>
        MapValue(v.map { case (key, value) => (key, Value.create(value)) })
      case v: Seq[Any] => ListValue(v.map(value => Value.create(value)).toList)
      case _ => throw new Exception(s"Invalid value: $value")
    }
  }
}
