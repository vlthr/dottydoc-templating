package vlthr.tee.filters
import scala.util.{Try, Success, Failure}
import scala.util.control.NonFatal
import vlthr.tee.parser.Liquid
import vlthr.tee.core._
import vlthr.tee.util._
import vlthr.tee.core.Error._
import scala.collection.mutable.{Map => MMap, Set => MSet}
import com.fasterxml.jackson.databind.ObjectMapper

object Filter {
  def byName(s: String): Option[Filter] = registry.get(s)
  val registry: MMap[String, Filter] = MMap(
    "split" -> Split(),
    "date" -> Date(),
    "slice" -> Slice(),
    "join" -> Join(),
    "size" -> Size(),
    "json" -> Json(),
    "first" -> First(),
    "last" -> Last(),
    "prepend" -> Prepend(),
    "append" -> Append(),
    "capitalize" -> Capitalize(),
    "downcase" -> Downcase(),
    "upcase" -> Upcase(),
    "escape" -> Escape(),
    "remove" -> Remove(),
    "replace" -> Replace(),
    "reverse" -> Reverse()
  )
}

abstract trait ArgSpec { self: Filter with OptArgSpec =>
  def checkArgs(args: List[Value])(implicit ctx: Context, parent: FilterExpr): List[Error]
}
abstract trait OptArgSpec { self: Filter =>
  def checkOptArgs(optArgs: List[Value])(implicit ctx: Context, parent: FilterExpr): List[Error]
}
abstract trait InputSpec { self: Filter =>
  def checkInput(input: Value)(implicit ctx: Context, parent: FilterExpr): List[Error]
}
abstract trait NoArgs { self: Filter =>
  def checkArgs(args: List[Value])(implicit ctx: Context, parent: FilterExpr): List[Error] = {
    val correctNumberOfArgs = args.size == 0
    if (!correctNumberOfArgs) List(InvalidFilterArgs(parent, this, args))
    else Nil
  }
}

abstract trait InputType(t: ValueType) extends InputSpec { self: Filter =>
  override def checkInput(input: Value)(implicit ctx: Context, parent: FilterExpr): List[Error] = {
    if (!t.matches(input)) InvalidFilterInput(parent, this, input) :: Nil
    else Nil
  }
}

abstract trait FixedOptArgs(types: ValueType*) extends OptArgSpec { self: Filter =>
  def checkOptArgs(optArgs: List[Value])(implicit ctx: Context, parent: FilterExpr): List[Error] = {
    val correctArgTypes = optArgs.zip(types).forall{ case (v, expected) => expected.matches(v) }
    val correctNumberOfArgs = optArgs.size == types.size
    if (!correctArgTypes || !correctNumberOfArgs) List(InvalidFilterArgs(parent, this, optArgs))
    else Nil
  }
}

abstract trait NoOptArgs extends OptArgSpec { self: Filter =>
  def checkOptArgs(optArgs: List[Value])(implicit ctx: Context, parent: FilterExpr): List[Error] = {
    val correctNumberOfArgs = optArgs.size == 0
    if (!correctNumberOfArgs) List(TooManyFilterArgs(parent, this, optArgs))
    else Nil
  }
}

abstract trait FixedArgs(types: ValueType*) extends ArgSpec { self: Filter with OptArgSpec =>
  // TODO: A macro could generate typesafe getters for each of the expected arguments.
  def checkArgs(allArgs: List[Value])(implicit ctx: Context, parent: FilterExpr): List[Error] = {
    var errors: List[Error] = Nil
    // Prune off optional arguments if applicable
    val args = if (allArgs.size > numArgs) {
      val (args, optArgs) = allArgs.splitAt(numArgs)
      errors = errors ++ checkOptArgs(optArgs)
      args
    } else allArgs
    val correctArgTypes = args.zip(types).forall{ case (v, expected) => expected.matches(v) }
    val correctNumberOfArgs = args.size == types.size
    if (!correctArgTypes || !correctNumberOfArgs) List(InvalidFilterArgs(parent, this, args))
    else Nil
  }
  def numArgs: Int = types.size
}

case class UnknownFilter(name: String) extends Filter {
  def filter(input: Value, args: List[Value])(
      implicit ctx: Context, parent: FilterExpr) = throw UnknownFilterNameException(name)
  def checkInput(v: Value)(implicit ctx: Context, parent: FilterExpr) = ???
  def checkArgs(v: List[Value])(implicit ctx: Context, parent: FilterExpr) = ???
}

case class Split() extends Filter with InputType(ValueType.String) with FixedArgs(ValueType.String) with NoOptArgs {
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

case class Join() extends Filter with InputType(ValueType.List) with FixedArgs(ValueType.String) with NoOptArgs {
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

case class Append() extends Filter with InputType(ValueType.String) with FixedArgs(ValueType.String) with NoOptArgs {
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

case class Prepend() extends Filter with InputType(ValueType.String) with FixedArgs(ValueType.String) with NoOptArgs {
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

case class Remove() extends Filter with InputType(ValueType.String) with FixedArgs(ValueType.String) with NoOptArgs {
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

case class Replace() extends Filter with InputType(ValueType.String) with FixedArgs(ValueType.String, ValueType.String) with NoOptArgs {
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

case class Date() extends Filter with InputType(ValueType.String | ValueType.Integer) with FixedArgs(ValueType.String) with NoOptArgs {
  import java.text.SimpleDateFormat
  import java.util.Locale

  def name = "date"
  val locale = Locale.ENGLISH
  val datePatterns: List[String] = List("yyyy-MM-dd HH:mm:ss", "EEE MMM dd hh:mm:ss yyyy")

  def liquidToJavaFormat = Map(
          // %% - Literal ``%'' character
        '%' -> new SimpleDateFormat("%", locale),

        // %a - The abbreviated weekday name (``Sun'')
        'a' -> new SimpleDateFormat("EEE", locale),

        // %A - The  full  weekday  name (``Sunday'')
        'A' -> new SimpleDateFormat("EEEE", locale),

        // %b - The abbreviated month name (``Jan'')
        'b' -> new SimpleDateFormat("MMM", locale),
        'h' -> new SimpleDateFormat("MMM", locale),

        // %B - The  full  month  name (``January'')
        'B' -> new SimpleDateFormat("MMMM", locale),

        // %c - The preferred local date and time representation
        'c' -> new SimpleDateFormat("EEE MMM dd HH:mm:ss yyyy", locale),

        // %d - Day of the month (01..31)
        'd' -> new SimpleDateFormat("dd", locale),

        // %H - Hour of the day, 24-hour clock (00..23)
        'H' -> new SimpleDateFormat("HH", locale),

        // %I - Hour of the day, 12-hour clock (01..12)
        'I' -> new SimpleDateFormat("hh", locale),

        // %j - Day of the year (001..366)
        'j' -> new SimpleDateFormat("DDD", locale),

        // %m - Month of the year (01..12)
        'm' -> new SimpleDateFormat("MM", locale),

        // %M - Minute of the hour (00..59)
        'M' -> new SimpleDateFormat("mm", locale),

        // %p - Meridian indicator (``AM''  or  ``PM'')
        'p' -> new SimpleDateFormat("a", locale),

        // %S - Second of the minute (00..60)
        'S' -> new SimpleDateFormat("ss", locale),

        // %U - Week  number  of the current year,
        //      starting with the first Sunday as the first
        //      day of the first week (00..53)
        'U' -> new SimpleDateFormat("ww", locale),

        // %W - Week  number  of the current year,
        //      starting with the first Monday as the first
        //      day of the first week (00..53)
        'W' -> new SimpleDateFormat("ww", locale),

        // %w - Day of the week (Sunday is 0, 0..6)
        'w' -> new SimpleDateFormat("F", locale),

        // %x - Preferred representation for the date alone, no time
        'x' -> new SimpleDateFormat("MM/dd/yy", locale),

        // %X - Preferred representation for the time alone, no date
        'X' -> new SimpleDateFormat("HH:mm:ss", locale),

        // %y - Year without a century (00..99)
        'y' -> new SimpleDateFormat("yy", locale),

        // %Y - Year with century
        'Y' -> new SimpleDateFormat("yyyy", locale),

        // %Z - Time zone name
        'Z' -> new SimpleDateFormat("z", locale))

  def toSeconds(date: String): Option[Long] = {
    datePatterns.map(pattern => Try(new SimpleDateFormat(pattern, locale).parse(date).getTime/1000L)).map(_.toOption).flatten.headOption
  }

  override def filter(input: Value, args: List[Value])(
    implicit ctx: Context, parent: FilterExpr) = {
    val seconds = input match {
      case StringValue(v) if v == "now" => System.currentTimeMillis / 1000L
      case StringValue(v) => toSeconds(v).getOrElse(throw FilterException(s"`$v` is not a valid date."))
      case IntValue(v) => v
      case v => throw FilterException(s"Unexpected Value Type: $v"); 1000L
    }
    val date = new java.util.Date(seconds * 1000L)

    val format = args(0).asInstanceOf[StringValue].v
    val calendar = java.util.Calendar.getInstance()
    calendar.setTime(date)

    val builder = new StringBuilder();

    var i = 0
    while (i < format.length) {
      val ch = format.charAt(i);
      if (ch == '%') {
        i+=1

        if (i == format.length()) {
          builder.append("%")
        }
        else {
          val next = format.charAt(i);

          val javaFormat = liquidToJavaFormat.get(next);

          javaFormat match {
            case Some(f) => builder.append(f.format(date))
            case _ => builder.append("%").append(next);
          }
        }
      }
      else {
        builder.append(ch);
      }
      i += 1
    }
    Success(StringValue(builder.toString))
  }
}

case class Slice() extends Filter with InputType(ValueType.String) with FixedArgs(ValueType.Integer) with FixedOptArgs(ValueType.Integer) {
  def name = "slice"
  override def filter(input: Value, args: List[Value])(
    // TODO: Support negative indexes
    implicit ctx: Context, parent: FilterExpr) = {
    val start = args(0).asInstanceOf[IntValue].v
    val stop = args.lift(1).map(_.asInstanceOf[IntValue].v).getOrElse(start)
    input match {
      case StringValue(v) => Success(StringValue(v.substring(start, stop+1)))
      case v => fail(UnexpectedValueType(parent, v))
    }
  }
}
