package vlthr.tee.core
import vlthr.tee.filters._
import vlthr.tee.core.Errors._
import shapeless._
import shapeless.ops.hlist.HKernelAux
import shapeless.ops.traversable._
import shapeless.syntax.typeable._
import validation.Result
import scala.collection.mutable.{ArrayBuffer, Map => MMap}

enum class ValueType {
  def matches(v: Value): Boolean
  def |(other: ValueType): ValueType = other match {
    case u @ ValueType.UnionType(_) => u | this
    case _ => ValueType.UnionType(Set(this, other))
  }
}
object ValueType {
  case Integer {
    def matches(v: Value) = v match {
      case v: IntValue => true
      case _ => false
    }
  }
  case String {
    def matches(v: Value) = v match {
      case StringValue(_) => true
      case _ => false
    }
  }
  case Boolean {
    def matches(v: Value) = v match {
      case v: BooleanValue => true
      case _ => false
    }
  }
  case List {
    def matches(v: Value) = v match {
      case v: ListValue => true
      case _ => false
    }
  }
  case Map {
    def matches(v: Value) = v match {
      case v: MapValue => true
      case _ => false
    }
  }
  case Null {
    def matches(v: Value) = v match {
      case v: NullValue => true
      case _ => false
    }
  }
  case UnionType(types: Set[ValueType]) {
    def matches(v: Value) = types.exists(_.matches(v))
    override def |(other: ValueType): ValueType = ValueType.UnionType(types + other)
  }
}

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

  def length: Int = body.length
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

object NoSourceFile extends SourceFile("", "") {
  override def length: Int = ???
  override def lineToOffset(index: Int): Int = ???
}
class NoPosition extends SourcePosition(-1, -1, NoSourceFile) {
  override def display: String = ???
  override def report: String = ???
}

object SourcePosition {
  def fromLine(sourceFile: SourceFile, line: Int, charPositionInLine: Int, length: Int) = {
    val start = sourceFile.lineToOffset(line) + charPositionInLine
    val stop = start + length
    SourcePosition(start, stop, sourceFile)
  }
}

case class ParseContext(sourcePosition: SourcePosition)

abstract trait ASTNode {
  // def parent: Option[ASTNode]
  val pctx: ParseContext
  def sourcePosition: SourcePosition = pctx.sourcePosition
  def render()(implicit ctx: Context): Validated[String]
}

abstract trait Obj extends ASTNode {
}

abstract trait Expr extends ASTNode {
  implicit val pctx: ParseContext
  def eval()(implicit ctx: Context): Validated[Value]
  def render()(implicit ctx: Context): Validated[String] = eval().flatMap(v => imbueFragments(v.render()))
}

abstract trait Extension {
  def name: String
  def extensionType: String
}

case class Context(mappings: MMap[String, Value],
                   customFilters: Map[String, Filter],
                   customTags: Map[String, Tag],
                   parent: Option[Context],
                   includeDir: String) {
  def lookup(s: String): Option[Value] =
    mappings.get(s).orElse(parent.flatMap(_.lookup(s)))
  def withParams(params: Map[String, Any]) = copy(mappings = mappings ++ Value.createMap(params))
  def withParamsMap(params: Map[String, Value]) = copy(mappings = mappings ++ params)
  def withFilter(filters: Filter*) = copy(customFilters = customFilters ++ filters.map(f => (f.name, f)))
  def withTag(tags: Tag*) = copy(customTags = customTags ++ tags.map(t => (t.name, t)))
  def withIncludeDir(includeDir: String) = copy(includeDir = includeDir)

  def getFilter(name: String): Filter = customFilters.get(name).orElse(Filter.byName(name)).getOrElse(unknownFilter(name))
  def getTag(name: String): Tag = customTags.get(name).getOrElse(UnknownTag(name))
}

object Context {
  type TagConstructor = (ParseContext, List[Expr]) => TagNode
  def createNew(): Context = Context(MMap(), Map(), Map(), None, "_include")
  def createChild(parent: Context): Context = parent.copy(parent=Some(parent))
}

sealed trait Truthable {
  def truthy: Boolean
}

trait Truthy extends Truthable {
  def truthy = true
}

sealed trait Value extends Truthable with Ordered[Value] {
  def display: String

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
}

sealed trait IndexedValue extends Value

final case class StringValue(v: String) extends Value with Truthy {
  def display: String = s""""$v""""
  def valueType = ValueType.String
  def render()(implicit ctx: Context): ValidatedFragment[String] = Result.valid(v)
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

final case class IntValue(v: Int) extends Value with Truthy {
  def display: String = s"""$v"""
  def valueType = ValueType.Integer
  def render()(implicit ctx: Context): ValidatedFragment[String] =
    Result.valid(v.toString)
  def get = v
}

final case class MapValue(v: Map[String, Value])
    extends IndexedValue
    with Truthy {
  def display: String = ???
  def valueType = ValueType.Map
  def render()(implicit ctx: Context): ValidatedFragment[String] = failFragment(UnrenderableValue(this))
  def get = v
}

final case class ListValue(v: List[Value]) extends IndexedValue with Truthy {
  def display: String = s"""[${v.map(_.display).mkString(", ")}]"""
  def valueType = ValueType.List
  def render()(implicit ctx: Context): ValidatedFragment[String] = failFragment(UnrenderableValue(this))
  def get = v
}

final case class NullValue() extends Value {
  def display: String = "null"
  def valueType = ValueType.Null
  def render()(implicit ctx: Context): ValidatedFragment[String] = succeed("null")
  def get = null
  def truthy = false
}

case class UnknownTag(n: String) extends Tag(n) {
  def checkArgs(v: List[Value])(implicit ctx: Context): List[ErrorFragment] = List(UnknownTagId(name))
  def render(args: List[Value])(
    implicit ctx: Context): String = ???
}

abstract trait Tag(val name: String) extends Extension {
  def extensionType = "tag"
  def checkArgs(v: List[Value])(implicit ctx: Context): List[ErrorFragment]
  def typeCheck(args: List[Value])(implicit ctx: Context): Validated[Unit] = {
    Result.valid(())
  }
  def render(args: List[Value])(
    implicit ctx: Context): String
}

object Value {
  def createMap(value: Map[String, Any]): Map[String, Value] = value.map {
    case (k: String, v: Any) => (k, Value.create(v))
  }
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

  /** Temporary fix for implicits not being created for union types
    */
  implicit val listStrTypeable: Typeable[ListValue | StringValue] =
    new Typeable[ListValue | StringValue] {
      def cast(t: Any): Option[ListValue | StringValue] = t match {
        case v: ListValue => Some(v)
        case v: StringValue => Some(v)
        case _ => None
      }

      def describe: String = s"BooleanValue"
    }

  /** Temporary fix for implicits not being created for union types
    */
  implicit val intStrTypeable: Typeable[IntValue | StringValue] =
    new Typeable[IntValue | StringValue] {
      def cast(t: Any): Option[IntValue | StringValue] = t match {
        case v: IntValue => Some(v)
        case v: StringValue => Some(v)
        case _ => None
      }

      def describe: String = s"BooleanValue"
    }

  /** Not working
    */
  implicit def unionTypeable[A, B](implicit aType: Typeable[A], bType: Typeable[B]): Typeable[A | B] = new Typeable[A | B] {
      def cast(t: Any): Option[A | B] = aType.cast(t).orElse(bType.cast(t))

      def describe: String = s"${aType.describe} | ${bType.describe}"
    }
}
