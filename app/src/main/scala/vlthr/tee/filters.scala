package vlthr.tee.filters
import scala.util.{Try, Success, Failure}
import vlthr.tee.parser.Liquid
import vlthr.tee.core._
import vlthr.tee.util._
import vlthr.tee.core.Error._
import scala.collection.mutable.{Map => MMap}
import com.fasterxml.jackson.databind.ObjectMapper

object Filter {
  def byName(s: String): Option[Filter] = registry.get(s)
  lazy val registry: MMap[String, Filter] =
    MMap(List(Split(),
              Join(),
              Size(),
              Json(),
              First(),
              Last(),
              Prepend(),
              Append(),
              Capitalize(),
              Downcase(),
              Upcase(),
              Escape(),
              Remove(),
              Replace(),
              Reverse()).map(f => (f.name, f)).toList: _*)
}

abstract trait NoArgs { self: Filter =>
  def checkArgs(args: List[Value])(implicit ctx: Context, parent: FilterExpr): List[Error] = {
    val correctNumberOfArgs = args.size == 0
    if (!correctNumberOfArgs) List(InvalidFilterArgs(parent, this, args))
    else Nil
  }
}

abstract trait InputType(t: ValueType) { self: Filter =>
  override def checkInput(input: Value)(implicit ctx: Context, parent: FilterExpr): List[Error] = {
    if (!t.matches(input)) InvalidFilterInput(parent, this, input) :: Nil
    else Nil
  }
}

abstract trait FixedArgs(types: List[ValueType]) { self: Filter =>
  // TODO: A macro could generate typesafe getters for each of the expected arguments.
  def checkArgs(args: List[Value])(implicit ctx: Context, parent: FilterExpr): List[Error] = {
    val correctArgTypes = args.zip(types).forall{ case (v, expected) => expected.matches(v) }
    val correctNumberOfArgs = args.size == types.size
    if (!correctArgTypes || !correctNumberOfArgs) List(InvalidFilterArgs(parent, this, args))
    else Nil
  }
}

abstract trait SingleArg(expected: ValueType) { self: Filter =>
  def checkArgs(args: List[Value])(implicit ctx: Context, parent: FilterExpr): List[Error] = {
    val correctNumberOfArgs = args.size == 1
    val correctArgTypes = if (correctNumberOfArgs) expected.matches(args(0)) else false
    if (!correctArgTypes || !correctNumberOfArgs) List(InvalidFilterArgs(parent, this, args))
    else Nil
  }
}

case class NoFilter() extends Filter {
  def name = "NoFilter"
  def filter(input: Value, args: List[Value])(
      implicit ctx: Context, parent: FilterExpr) = ???
  def checkInput(v: Value)(implicit ctx: Context, parent: FilterExpr) = ???
  def checkArgs(v: List[Value])(implicit ctx: Context, parent: FilterExpr) = ???
}

case class Split() extends Filter with InputType(ValueType.String) with SingleArg(ValueType.String) {
  def name = "split"
  override def filter(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    val pattern = args(0).asInstanceOf[StringValue]
    input match {
      case StringValue(v) => Try(Value.create(v.split(pattern.v).toList))
      case v => fail(UnexpectedValueType(parent, v))
    }
  }
}

case class Json() extends Filter with InputType(ValueType.List) with NoArgs {
  def name = "json"
  override def filter(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = Success(StringValue(new ObjectMapper().writeValueAsString(Util.asJava(input.asInstanceOf[ListValue]))))
}

case class Size() extends Filter with InputType(ValueType.List | ValueType.String) with NoArgs {
  def name = "size"
  override def filter(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = input match  {
    case StringValue(v) => Try(IntValue(v.size))
    case ListValue(v) => Try(IntValue(v.size))
    case v => fail(UnexpectedValueType(parent, v))
  }
}

case class First() extends Filter with InputType(ValueType.List | ValueType.String) with NoArgs {
  def name = "first"
  override def filter(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = input match  {
    case StringValue(v) => Try(StringValue(""+v.head))
    case ListValue(v) => Try(v.head)
    case v => fail(UnexpectedValueType(parent, v))
  }
}

case class Last() extends Filter with InputType(ValueType.List | ValueType.String) with NoArgs {
  def name = "last"
  override def filter(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = input match  {
    case StringValue(v) => Try(StringValue(""+v.last))
    case ListValue(v) => Try(v.last)
    case v => fail(UnexpectedValueType(parent, v))
  }
}

case class Reverse() extends Filter with InputType(ValueType.List | ValueType.String) with NoArgs {
  def name = "reverse"
  override def filter(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = input match  {
    case StringValue(v) => Try(StringValue(v.reverse))
    case ListValue(v) => Try(ListValue(v.reverse))
    case v => fail(UnexpectedValueType(parent, v))
  }
}

case class Join() extends Filter with InputType(ValueType.List) with SingleArg(ValueType.String) {
  def name = "join"
  override def filter(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    val delim = args(0).asInstanceOf[StringValue]
    input match  {
      case ListValue(v) => Try(StringValue(v.map(_.render.get).mkString(delim.v)))
      case v => fail(UnexpectedValueType(parent, v))
    }
  }
}

case class Capitalize() extends Filter with InputType(ValueType.String) with NoArgs {
  def name = "capitalize"
  override def filter(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    input match  {
      case StringValue(v) => Try(StringValue(Character.toUpperCase(v(0)) + v.substring(1)))
      case v => fail(UnexpectedValueType(parent, v))
    }
  }
}

case class Downcase() extends Filter with InputType(ValueType.String) with NoArgs {
  def name = "downcase"
  override def filter(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    input match  {
      case StringValue(v) => Try(StringValue(v.map(c => Character.toLowerCase(c)).mkString))
      case v => fail(UnexpectedValueType(parent, v))
    }
  }
}

case class Upcase() extends Filter with InputType(ValueType.String) with NoArgs {
  def name = "upcase"
  override def filter(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    input match  {
      case StringValue(v) => Try(StringValue(v.map(c => Character.toUpperCase(c)).mkString))
      case v => fail(UnexpectedValueType(parent, v))
    }
  }
}

case class Append() extends Filter with InputType(ValueType.String) with SingleArg(ValueType.String) {
  def name = "append"
  override def filter(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    val end = args(0).asInstanceOf[StringValue]
    input match  {
      case StringValue(v) => Try(StringValue(v + end.v))
      case v => fail(UnexpectedValueType(parent, v))
    }
  }
}

case class Prepend() extends Filter with InputType(ValueType.String) with SingleArg(ValueType.String) {
  def name = "prepend"
  override def filter(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    val start = args(0).asInstanceOf[StringValue]
    input match  {
      case StringValue(v) => Try(StringValue(start.v + v))
      case v => fail(UnexpectedValueType(parent, v))
    }
  }
}

case class Escape() extends Filter with InputType(ValueType.String) with NoArgs {
  def name = "escape"
  override def filter(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    input match  {
      case StringValue(v) => Try(StringValue(v.replace("<", "&lt;")
                                               .replace(">", "&gt;")
                                               .replace("\"", "&quot;")
                                               .replace("&", "&amp;")))
      case v => fail(UnexpectedValueType(parent, v))
    }
  }
}

case class Remove() extends Filter with InputType(ValueType.String) with SingleArg(ValueType.String) {
  def name = "remove"
  override def filter(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    val pattern = args(0).asInstanceOf[StringValue]
    input match  {
      case StringValue(v) => Try(StringValue(v.replace(pattern.v, "")))
      case v => fail(UnexpectedValueType(parent, v))
    }
  }
}

case class Replace() extends Filter with InputType(ValueType.String) with FixedArgs(ValueType.String :: ValueType.String :: Nil) {
  def name = "replace"
  override def filter(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    val pattern = args(0).asInstanceOf[StringValue]
    val replacement = args(1).asInstanceOf[StringValue]
    input match  {
      case StringValue(v) => Try(StringValue(v.replace(pattern.v, replacement.v)))
      case v => fail(UnexpectedValueType(parent, v))
    }
  }
}
