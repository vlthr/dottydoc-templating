package com.vlthr.levee.core
import com.vlthr.levee.filters._
import com.vlthr.levee.util._
import com.vlthr.levee.parser.Liquid
import com.vlthr.levee.core.error._
import shapeless._
import shapeless.ops.hlist.HKernelAux
import shapeless.ops.traversable._
import shapeless.syntax.typeable._
import validation.Result
import scala.util.{Success, Failure, Try}
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scala.collection.JavaConverters._

/** Represents a template file.
  *
  * @constructor create a new source file
  * @param body the content of the template file
  * @param path the path to the template file
  */
case class SourceFile(body: String, path: String) {
  final val LF = '\u000A'
  final val FF = '\u000C'
  final val CR = '\u000D'
  final val SU = '\u001A'

  /** Is character a line break? */
  def isLineBreakChar(c: Char) = c match {
    case LF | FF | CR | SU => true
    case _ => false
  }

  private def isLineBreak(idx: Int) =
    if (idx >= length) false
    else {
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

  def length: Int = body.length
}

/** Represents a range of characters in a given source file
  *
  * @constructor create a new source position
  * @param start the index of the first character in the template
  * @param end the index of the last character in the template (inclusive)
  * @param template the source file this source position is in
  */
case class SourcePosition(start: Int, end: Int, template: SourceFile) {
  def display: String = template.body.substring(start, end + 1)
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

object NoSourceFile extends SourceFile("", "") {
  override def length: Int = ???
  override def lineToOffset(index: Int): Int = ???
}

class NoPosition extends SourcePosition(-1, -1, NoSourceFile) {
  override def display: String = ???
  override def report: String = ???
}

object SourcePosition {
  def fromLine(sourceFile: SourceFile,
               line: Int,
               charPositionInLine: Int,
               length: Int) = {
    val start = sourceFile.lineToOffset(line) + charPositionInLine
    val stop = start + length
    SourcePosition(start, stop, sourceFile)
  }
}

/** Holds metadata relating to how and where a given AST node was parsed
  *
  * @constructor
  * @param sourcePosition source position for the AST node
  */
case class ParseContext(sourcePosition: SourcePosition)

/** Common trait for all AST nodes. */
abstract trait ASTNode {
  // def parent: Option[ASTNode]
  val pctx: ParseContext
  def sourcePosition: SourcePosition = pctx.sourcePosition
  def render()(implicit ctx: Context): Validated[String]
}

/** Common trait for all object nodes, i.e. Liquid objects of the form
  * {{ ... }} or {% ... %}
  */
abstract trait Obj extends ASTNode

/** Common trait for all nodes that can be evaluated to produce a Value */
abstract trait Expr extends ASTNode {
  implicit val pctx: ParseContext
  def eval()(implicit ctx: Context): Validated[Value]
  def render()(implicit ctx: Context): Validated[String] =
    eval().flatMap(v => imbueFragments(v.render()))
}

/** Common trait for code that is not directly part of the AST, i.e. tags and filters  */
abstract trait Extension {
  def name: String
  def extensionType: String
}

/** Holds all state (except variable bindings) that may be mutated during rendering or evaluation */
case class ExecutionState(var breakWasHit: Boolean = false,
                          var continueWasHit: Boolean = false)

/** Holds configuration and global variables required for rendering and evaluation
  *
  * @param mappings the variable bindings used when rendering the template - may be mutated
  * @param customFilters all registered custom filters as a map from filter name to filter object
  * @param customTags all registered custom tags as a map from tag name to tag object
  * @param parent a reference to a parent context - used to keep track of nested scopes
  * @param includeDir the path to the directory where included snippets can be found
  * @param executionState the current execution state
  */
case class Context(mappings: MMap[String, Value],
                   customFilters: Map[String, Filter],
                   customTags: Map[String, Tag],
                   parent: Option[Context],
                   includeDir: String,
                   var executionState: ExecutionState) {

  /** Check whether a given variable name is bound to a value */
  def lookup(s: String): Option[Value] =
    mappings.get(s).orElse(parent.flatMap(_.lookup(s)))

  /** Add new variable bindings to the set of defined variables.
    * @param params A map of variable name to value. Automatically converts values to appropriate Value objects */
  def withParams(params: Map[String, Any]) =
    copy(mappings = mappings ++ Value.createMap(params))

  /** Add new variable bindings to the set of defined variables
    * @param params A map of variable name to value. */
  def withParamsMap(params: Map[String, Value]) =
    copy(mappings = mappings ++ params)

  /** Adds now custom filter to the set of registered filters
    * @param filters one or more Filter objects */
  def withFilter(filters: Filter*) =
    copy(customFilters = customFilters ++ filters.map(f => (f.name, f)))

  /** Adds now custom tag to the set of registered tags
    * @param tags one or more Tag objects */
  def withTag(tags: Tag*) =
    copy(customTags = customTags ++ tags.map(t => (t.name, t)))

  /** Sets the include directory path */
  def withIncludeDir(includeDir: String) = copy(includeDir = includeDir)

  /** Gets a filter object by name if one exists */
  def getFilter(name: String): Filter =
    customFilters
      .get(name)
      .orElse(Filter.byName(name))
      .getOrElse(unknownFilter(name))

  /** Gets a tag object by name if one exists */
  def getTag(name: String): Tag =
    customTags.get(name).getOrElse(UnknownTag(name))

  /** Renders a file using this context. */
  def renderFile(file: SourceFile): Try[String] = {
    implicit val c = this
    this.executionState = ExecutionState()
    toTry(Liquid.parse(file).flatMap(_.render()))
  }

  /** Renders a file using this context. */
  def renderFile(path: String): Try[String] =
    renderFile(SourceFile(Util.readWholeFile(path), path))

  /** Renders a template string using this context. */
  def renderString(body: String): Try[String] =
    renderFile(SourceFile(body, "In-memory file"))
}

object Context {
  type TagConstructor = (ParseContext, List[Expr]) => TagNode

  /** Create a new context */
  def createNew(): Context =
    Context(MMap(), Map(), Map(), None, "_include", ExecutionState())

  /** Create a new context as a child to the given context.
    *
    * Used to implement variable scoping.
    */
  def createChild(parent: Context): Context =
    parent.copy(parent = Some(parent), executionState = ExecutionState())
}

class ValueType(name: String)
object ValueType {
  case object String extends ValueType("String")
  case object Integer extends ValueType("Integer")
  case object Boolean extends ValueType("Boolean")
  case object Map extends ValueType("Map")
  case object List extends ValueType("List")
  case object Null extends ValueType("Null")
}

/** Represents the allowed types of values in the language */
sealed trait Value extends Ordered[Value] {
  /** Display a value literal for use in debugging. */
  def display: String

  /** Render the value to a string */
  def render()(implicit ctx: Context): ValidatedFragment[String]

  def valueType: ValueType

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

  def truthy = false
}

sealed trait IndexedValue extends Value

final case class StringValue(v: String) extends Value {
  def display: String = s""""$v""""
  def valueType = ValueType.String
  def render()(implicit ctx: Context): ValidatedFragment[String] =
    Result.valid(v)
  def truthy = true
  def get = v
}

final case class BooleanValue(v: Boolean) extends Value {
  def display: String = s"""$v"""
  def valueType = ValueType.Boolean
  def render()(implicit ctx: Context): ValidatedFragment[String] =
    Result.valid(v.toString)
  def truthy = v
  def get = v
}

final case class IntValue(v: Int) extends Value {
  def display: String = s"""$v"""
  def valueType = ValueType.Integer
  def render()(implicit ctx: Context): ValidatedFragment[String] =
    Result.valid(v.toString)
  def truthy = true
  def get = v
}

/** Represents nested variables that can be accessed using a DotExpr, i.e. {{ a.b.c }} */
final case class MapValue(v: Map[String, Value])
    extends IndexedValue {
  def display: String = ???
  def valueType = ValueType.Map
  def render()(implicit ctx: Context): ValidatedFragment[String] =
    failure(UnrenderableValue(this))
  def truthy = true
  def get = v
}

final case class ListValue(v: List[Value]) extends IndexedValue {
  def display: String = s"""[${v.map(_.display).mkString(", ")}]"""
  def valueType = ValueType.List
  def render()(implicit ctx: Context): ValidatedFragment[String] =
    failure(UnrenderableValue(this))
  def truthy = true
  def get = v
}

final case class NullValue() extends Value {
  def display: String = "null"
  def valueType = ValueType.Null
  def render()(implicit ctx: Context): ValidatedFragment[String] =
    success("null")
  def truthy = false
  def get = null
}

case class UnknownTag(n: String) extends Tag(n) {
  def checkArgs(v: List[Value])(implicit ctx: Context): List[ErrorFragment] =
    List(UnknownTagId(name))
  def render(args: List[Value])(implicit ctx: Context): String = ???
}

/** Base class for custom tags, i.e. {% myTag %} */
abstract class Tag(val name: String) extends Extension {
  def extensionType = "tag"
  def checkArgs(v: List[Value])(implicit ctx: Context): List[ErrorFragment]
  def typeCheck(args: List[Value])(implicit ctx: Context): Validated[Unit] = {
    Result.valid(())
  }
  def render(args: List[Value])(implicit ctx: Context): String
}

object Value {
  /** Convert a map of non-levee values to one with levee values. */
  def createMap(value: java.util.Map[String, Any]): Map[String, Value] =
    value.asScala.toMap.map {
      case (k: String, v: Any) => (k, Value.create(v))
    }

  /** Convert a map of non-levee values to one with levee values. */
  def createMap(value: Map[String, Any]): Map[String, Value] = value.map {
    case (k: String, v: Any) => (k, Value.create(v))
  }

  /** Convert a non-levee value a levee value. */
  def create(value: Any): Value = {
    value match {
      case v: Value => v
      case v: Int => IntValue(v)
      case v: String => StringValue(v)
      case v: java.lang.String => StringValue(v)
      case v: Boolean => BooleanValue(v)
      case v: Char => StringValue("" + v)
      case v: java.util.Map[String, _] =>
        MapValue(v.asScala.toMap.map {
          case (key, value) => (key, Value.create(value))
        })
      case v: Map[String, _] =>
        MapValue(v.map { case (key, value) => (key, Value.create(value)) })
      case v: Seq[_] => ListValue(v.map(value => Value.create(value)).toList)
      case _ =>
        throw new Exception(s"Invalid value: $value, ${value.toString}")
    }
  }

  implicit val intTypeable: Typeable[IntValue] =
    new Typeable[IntValue] {
      def cast(t: Any): Option[IntValue] = t match {
        case c: IntValue => Some(c)
        case _ => None
      }

      def describe: String = s"IntValue"
    }

  implicit val stringTypeable: Typeable[StringValue] =
    new Typeable[StringValue] {
      def cast(t: Any): Option[StringValue] = t match {
        case v: StringValue => Some(v)
        case _ => None
      }

      def describe: String = s"StringValue"
    }

  implicit val mapTypeable: Typeable[MapValue] =
    new Typeable[MapValue] {
      def cast(t: Any): Option[MapValue] = t match {
        case v: MapValue => Some(v)
        case _ => None
      }

      def describe: String = s"MapValue"
    }

  implicit val listTypeable: Typeable[ListValue] =
    new Typeable[ListValue] {
      def cast(t: Any): Option[ListValue] = t match {
        case v: ListValue => Some(v)
        case _ => None
      }

      def describe: String = s"ListValue"
    }

  implicit val boolTypeable: Typeable[BooleanValue] =
    new Typeable[BooleanValue] {
      def cast(t: Any): Option[BooleanValue] = t match {
        case v: BooleanValue => Some(v)
        case _ => None
      }

      def describe: String = s"BooleanValue"
    }

  implicit val nullTypeable: Typeable[NullValue] =
    new Typeable[NullValue] {
      def cast(t: Any): Option[NullValue] = t match {
        case v: NullValue => Some(v)
        case _ => None
      }

      def describe: String = s"BooleanValue"
    }
}
