package vlthr.tee.filters
import scala.util.{Try, Success, Failure}
import vlthr.tee.parser.Liquid
import vlthr.tee.core._
import vlthr.tee.util._
import vlthr.tee.core.Error._
import com.fasterxml.jackson.databind.ObjectMapper

object Filter {
  def byName(s: String): Constructor = registry.get(s).getOrElse((args, sp) => NoFilter()(sp))
  type Constructor = (List[Value], ParseContext) => Filter
  var registry: scala.collection.mutable.Map[String, Constructor] = scala.collection.mutable.Map(
    "split" -> ((args, sp) => Split(args)(sp)),
    "join" -> ((args, sp) => Join(args)(sp)),
    "size" -> ((args, sp) => Size(args)(sp)),
    "json" -> ((args, sp) => Json(args)(sp)),
    "first" -> ((args, sp) => First(args)(sp)),
    "last" -> ((args, sp) => Last(args)(sp)),
    "prepend" -> ((args, sp) => Prepend(args)(sp)),
    "append" -> ((args, sp) => Append(args)(sp)),
    "capitalize" -> ((args, sp) => Capitalize(args)(sp)),
    "downcase" -> ((args, sp) => Downcase(args)(sp)),
    "upcase" -> ((args, sp) => Upcase(args)(sp)),
    "escape" -> ((args, sp) => Escape(args)(sp)),
    "remove" -> ((args, sp) => Remove(args)(sp)),
    "replace" -> ((args, sp) => Replace(args)(sp)),
    "reverse" -> ((args, sp) => Reverse(args)(sp))
  )
  def register(name: String, f: Constructor): Unit = registry.put(name, f)
}

abstract trait NoArgs { self: Filter =>
  def checkArgs(args: List[Value]): List[Error] = {
    val correctNumberOfArgs = args.size == 0
    if (!correctNumberOfArgs) List(InvalidFilterArgs(this, args))
    else Nil
  }
}

abstract trait InputType(t: ValueType) { self: Filter =>
  override def checkInput(input: Value): List[Error] = {
    if (!t.matches(input)) InvalidFilterInput(this, input) :: Nil
    else Nil
  }
}

abstract trait FixedArgs(types: List[ValueType]) { self: Filter =>
  // TODO: A macro could generate typesafe getters for each of the expected arguments.
  def checkArgs(args: List[Value]): List[Error] = {
    val correctArgTypes = args.zip(types).forall{ case (v, expected) => expected.matches(v) }
    val correctNumberOfArgs = args.size == types.size
    if (!correctArgTypes || !correctNumberOfArgs) List(InvalidFilterArgs(this, args))
    else Nil
  }
}

abstract trait SingleArg(expected: ValueType) { self: Filter =>
  def checkArgs(args: List[Value]): List[Error] = {
    val correctNumberOfArgs = args.size == 1
    val correctArgTypes = if (correctNumberOfArgs) expected.matches(args(0)) else false
    if (!correctArgTypes || !correctNumberOfArgs) List(InvalidFilterArgs(this, args))
    else Nil
  }
}

case class NoFilter()(implicit val pctx: ParseContext) extends Filter(Nil) {
  def name = "NoFilter"
  def apply(input: Value, args: List[Value])(
      implicit ctx: Context, parent: FilterExpr) = ???
  def checkInput(v: Value) = ???
  def checkArgs(v: List[Value]) = ???
}

case class Split(args: List[Value])(implicit val pctx: ParseContext) extends Filter(args) with InputType(ValueType.String) with SingleArg(ValueType.String) {
  def name = "split"
  override def apply(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    val pattern = args(0).asInstanceOf[StringValue]
    input match {
      case StringValue(v) => Try(Value.create(v.split(pattern.v).toList))
      case v => fail(UnexpectedValueType(v))
    }
  }
}

case class Json(args: List[Value])(implicit override val pctx: ParseContext) extends Filter(args) with InputType(ValueType.List) with NoArgs {
  def name = "json"
  override def apply(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = Success(StringValue(new ObjectMapper().writeValueAsString(Util.asJava(input.asInstanceOf[ListValue]))))
}

case class Size(args: List[Value])(implicit val pctx: ParseContext) extends Filter(args) with InputType(ValueType.List | ValueType.String) with NoArgs {
  def name = "size"
  override def apply(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = input match  {
    case StringValue(v) => Try(IntValue(v.size))
    case ListValue(v) => Try(IntValue(v.size))
    case v => fail(UnexpectedValueType(v))
  }
}

case class First(args: List[Value])(implicit val pctx: ParseContext) extends Filter(args) with InputType(ValueType.List | ValueType.String) with NoArgs {
  def name = "first"
  override def apply(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = input match  {
    case StringValue(v) => Try(StringValue(""+v.head))
    case ListValue(v) => Try(v.head)
    case v => fail(UnexpectedValueType(v))
  }
}

case class Last(args: List[Value])(implicit val pctx: ParseContext) extends Filter(args) with InputType(ValueType.List | ValueType.String) with NoArgs {
  def name = "last"
  override def apply(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = input match  {
    case StringValue(v) => Try(StringValue(""+v.last))
    case ListValue(v) => Try(v.last)
    case v => fail(UnexpectedValueType(v))
  }
}

case class Reverse(args: List[Value])(implicit val pctx: ParseContext) extends Filter(args) with InputType(ValueType.List | ValueType.String) with NoArgs {
  def name = "reverse"
  override def apply(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = input match  {
    case StringValue(v) => Try(StringValue(v.reverse))
    case ListValue(v) => Try(ListValue(v.reverse))
    case v => fail(UnexpectedValueType(v))
  }
}

case class Join(args: List[Value])(implicit val pctx: ParseContext) extends Filter(args) with InputType(ValueType.List) with SingleArg(ValueType.String) {
  def name = "join"
  override def apply(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    val delim = args(0).asInstanceOf[StringValue]
    input match  {
      case ListValue(v) => Try(StringValue(v.map(_.render.get).mkString(delim.v)))
      case v => fail(UnexpectedValueType(v))
    }
  }
}

case class Capitalize(args: List[Value])(implicit val pctx: ParseContext) extends Filter(args) with InputType(ValueType.String) with NoArgs {
  def name = "capitalize"
  override def apply(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    input match  {
      case StringValue(v) => Try(StringValue(Character.toUpperCase(v(0)) + v.substring(1)))
      case v => fail(UnexpectedValueType(v))
    }
  }
}

case class Downcase(args: List[Value])(implicit val pctx: ParseContext) extends Filter(args) with InputType(ValueType.String) with NoArgs {
  def name = "downcase"
  override def apply(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    input match  {
      case StringValue(v) => Try(StringValue(v.map(c => Character.toLowerCase(c)).mkString))
      case v => fail(UnexpectedValueType(v))
    }
  }
}

case class Upcase(args: List[Value])(implicit val pctx: ParseContext) extends Filter(args) with InputType(ValueType.String) with NoArgs {
  def name = "upcase"
  override def apply(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    input match  {
      case StringValue(v) => Try(StringValue(v.map(c => Character.toUpperCase(c)).mkString))
      case v => fail(UnexpectedValueType(v))
    }
  }
}

case class Append(args: List[Value])(implicit val pctx: ParseContext) extends Filter(args) with InputType(ValueType.String) with SingleArg(ValueType.String) {
  def name = "append"
  override def apply(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    val end = args(0).asInstanceOf[StringValue]
    input match  {
      case StringValue(v) => Try(StringValue(v + end.v))
      case v => fail(UnexpectedValueType(v))
    }
  }
}

case class Prepend(args: List[Value])(implicit val pctx: ParseContext) extends Filter(args) with InputType(ValueType.String) with SingleArg(ValueType.String) {
  def name = "prepend"
  override def apply(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    val start = args(0).asInstanceOf[StringValue]
    input match  {
      case StringValue(v) => Try(StringValue(start.v + v))
      case v => fail(UnexpectedValueType(v))
    }
  }
}

case class Escape(args: List[Value])(implicit val pctx: ParseContext) extends Filter(args) with InputType(ValueType.String) with NoArgs {
  def name = "escape"
  override def apply(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    input match  {
      case StringValue(v) => Try(StringValue(v.replace("<", "&lt;")
                                               .replace(">", "&gt;")
                                               .replace("\"", "&quot;")
                                               .replace("&", "&amp;")))
      case v => fail(UnexpectedValueType(v))
    }
  }
}

case class Remove(args: List[Value])(implicit val pctx: ParseContext) extends Filter(args) with InputType(ValueType.String) with SingleArg(ValueType.String) {
  def name = "remove"
  override def apply(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    val pattern = args(0).asInstanceOf[StringValue]
    input match  {
      case StringValue(v) => Try(StringValue(v.replace(pattern.v, "")))
      case v => fail(UnexpectedValueType(v))
    }
  }
}

case class Replace(args: List[Value])(implicit val pctx: ParseContext) extends Filter(args) with InputType(ValueType.String) with FixedArgs(ValueType.String :: ValueType.String :: Nil) {
  def name = "replace"
  override def apply(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    val pattern = args(0).asInstanceOf[StringValue]
    val replacement = args(1).asInstanceOf[StringValue]
    input match  {
      case StringValue(v) => Try(StringValue(v.replace(pattern.v, replacement.v)))
      case v => fail(UnexpectedValueType(v))
    }
  }
}
