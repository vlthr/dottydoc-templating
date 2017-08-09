package vlthr.tee
import scala.util.{Try, Success, Failure}
import scala.util.control.NonFatal
import vlthr.tee.parser.Liquid
import vlthr.tee.core._
import vlthr.tee.util._
import vlthr.tee.core.Errors._
import validation.Result
import vlthr.tee.typetraits.TypeTraits._
import scala.collection.mutable.{Map => MMap, Set => MSet}
import com.fasterxml.jackson.databind.ObjectMapper
import shapeless._
import shapeless.ops.hlist.HKernelAux
import shapeless.ops.traversable._
import shapeless.syntax.typeable._

package object filters {
  // implicit def hnilFromTraversable[T]: FromTraversable[HNil] = FromTraversable.hnilFromTraversable

  // implicit def hlistFromTraversable[OutH, OutT <: HList]
  //   (implicit flt : FromTraversable[OutT], oc : Typeable[OutH]): FromTraversable[OutH :: OutT] = FromTraversable.hlistFromTraversable

  def makeFilter[I, A <: HList, O <: HList](name: String)(f: (Context, Filter, I, A, O) => ValidatedFragment[Value])(implicit ftArgs: FromTraversable[A], hkArgs: HKernelAux[A], ftOpt: FromTraversable[O]) = new Filter {
    type Input = I
    type Args = A
    type OptArgs = O
    def name = name
    def filter(input: Input, args: Args, optArgs: OptArgs)(implicit ctx: Context): ValidatedFragment[Value] = f(ctx, this, input, args, optArgs)
    def apply(input: Value, allArgs: List[Value])(implicit ctx: Context): ValidatedFragment[Value] = {
      val (args, optArgs) = allArgs.splitAt(intLen[Args])
      val i = input.cast[Input].get
      val a = ftArgs(args).get
      val o = ftOpt(optArgs).get
      filter(i, a, o)
    }
  }
  abstract trait Filter extends Extension {
    type Input
    type Args <: HList
    type OptArgs <: HList
    def name: String
    def filter(input: Input, args: Args, optArgs: OptArgs)(implicit ctx: Context): ValidatedFragment[Value]
    def extensionType = "filter"
    def intLen[T <: HList](implicit ker: HKernelAux[T]): Int = ker().length
    def apply(input: Value, allArgs: List[Value])(implicit ctx: Context): ValidatedFragment[Value]
  }

  object Filter {
    def byName(s: String): Option[Filter] = registry.get(s)
    val registry: MMap[String, Filter] = MMap(
      // "split" -> Split(),
      // "json" -> Json(),
      // "date" -> Date(),
      // "slice" -> Slice(),
      // "join" -> Join(),
      // "size" -> Size(),
      // "first" -> First(),
      // "last" -> Last(),
      // "prepend" -> Prepend(),
      // "append" -> Append(),
      // "capitalize" -> Capitalize(),
      // "downcase" -> Downcase(),
      // "upcase" -> Upcase(),
      // "escape" -> Escape(),
      // "remove" -> Remove(),
      // "replace" -> Replace(),
      // "reverse" -> Reverse()
    )
  }

  def unknownFilter(name: String) = makeFilter[HNil, HNil, HNil](name) { (ctx, self, input, args, optArgs) =>
    failFragment(UnknownFilterName(self.name))
  }

// case class Split() extends Filter {
//   def name = "split"
//   type Input = StringValue
//   type Args = StringValue :: HNil
//   type OptArgs = HNil
//   def filter(input: Input, args: Args, optArgs: OptArgs)(
//       implicit ctx: Context) = {
//     val pattern = args.head.get
//     val stringToSplit = input.get
//     succeed(Value.create(stringToSplit.split(pattern).toList))
//   }
// }

// case class Json() extends Filter {
//   def name = "json"
//   type Input = ListValue
//   type Args = HNil
//   type OptArgs = HNil
//   def filter(input: Input, args: Args, optArgs: OptArgs)(
//       implicit ctx: Context) = {
//     succeed(
//       StringValue(
//         new ObjectMapper()
//           .writeValueAsString(Util.asJava(input.asInstanceOf[ListValue]))))
//   }
// }

// case class Size()
//     extends Filter
//     with InputType(ValueType.List | ValueType.String)
//     with NoArgs
//     with NoKwArgs {
//   def name = "size"
//   override def filter(input: Value,
//                       args: List[Value],
//                       kwargs: Map[String, Value])(implicit ctx: Context) =
//     input match {
//       case StringValue(v) => Try(IntValue(v.size))
//       case ListValue(v) => Try(IntValue(v.size))
//       case v => failFragment(UnexpectedValueType(v))
//     }
// }

// case class First()
//     extends Filter
//     with InputType(ValueType.List | ValueType.String)
//     with NoArgs
//     with NoKwArgs {
//   def name = "first"
//   override def filter(input: Value,
//                       args: List[Value],
//                       kwargs: Map[String, Value])(implicit ctx: Context) =
//     input match {
//       case StringValue(v) => Try(StringValue("" + v.head))
//       case ListValue(v) => Try(v.head)
//       case v => failFragment(UnexpectedValueType(v))
//     }
// }

// case class Last()
//     extends Filter
//     with InputType(ValueType.List | ValueType.String)
//     with NoArgs
//     with NoKwArgs {
//   def name = "last"
//   override def filter(input: Value,
//                       args: List[Value],
//                       kwargs: Map[String, Value])(implicit ctx: Context) =
//     input match {
//       case StringValue(v) => Try(StringValue("" + v.last))
//       case ListValue(v) => Try(v.last)
//       case v => failFragment(UnexpectedValueType(v))
//     }
// }

case class Reverse()
    extends Filter
    with InputType(ValueType.List | ValueType.String)
    with NoArgs
    with NoKwArgs {
  def name = "reverse"
  // type Input = StringValue :+: ListValue :+: CNil
  type Input = StringValue | ListValue
  type Args = StringValue :: HNil
  type OptArgs = HNil
  def filter(input: Input, args: Args, optArgs: OptArgs)(
      implicit ctx: Context) = {
    input match {
      case StringValue(str) => succeed(StringValue(str.reverse))
      case ListValue(list) => succeed(ListValue(list.reverse))
    }

    // input match {
    //   case Inl(str) => succeed(StringValue(str.get.reverse))
    //   case Inr(Inl(list)) => succeed(ListValue(list.get.reverse))
    //   case _ => ???
    // }
  }
}

// case class Join()
//     extends Filter
//     with InputType(ValueType.List)
//     with FixedArgs(ValueType.String)
//     with NoOptArgs
//     with NoKwArgs {
//   def name = "join"
//   override def filter(input: Value,
//                       args: List[Value],
//                       kwargs: Map[String, Value])(implicit ctx: Context) = {
//     val delim = args(0).asInstanceOf[StringValue]
//     input match {
//       case ListValue(v) =>
//         Try(StringValue(v.map(_.render().get).mkString(delim.v)))
//       case v => failFragment(UnexpectedValueType(v))
//     }
//   }
// }

// case class Capitalize()
//     extends Filter
//     with InputType(ValueType.String)
//     with NoArgs
//     with NoKwArgs {
//   def name = "capitalize"
//   override def filter(input: Value,
//                       args: List[Value],
//                       kwargs: Map[String, Value])(implicit ctx: Context) = {
//     input match {
//       case StringValue(v) =>
//         Try(StringValue(Character.toUpperCase(v(0)) + v.substring(1)))
//       case v => failFragment(UnexpectedValueType(v))
//     }
//   }
// }

// case class Downcase()
//     extends Filter
//     with InputType(ValueType.String)
//     with NoArgs
//     with NoKwArgs {
//   def name = "downcase"
//   override def filter(input: Value,
//                       args: List[Value],
//                       kwargs: Map[String, Value])(implicit ctx: Context) = {
//     input match {
//       case StringValue(v) =>
//         Try(StringValue(v.map(c => Character.toLowerCase(c)).mkString))
//       case v => failFragment(UnexpectedValueType(v))
//     }
//   }
// }

// case class Upcase()
//     extends Filter
//     with InputType(ValueType.String)
//     with NoArgs
//     with NoKwArgs {
//   def name = "upcase"
//   override def filter(input: Value,
//                       args: List[Value],
//                       kwargs: Map[String, Value])(implicit ctx: Context) = {
//     input match {
//       case StringValue(v) =>
//         Try(StringValue(v.map(c => Character.toUpperCase(c)).mkString))
//       case v => failFragment(UnexpectedValueType(v))
//     }
//   }
// }

// case class Append()
//     extends Filter
//     with InputType(ValueType.String)
//     with FixedArgs(ValueType.String)
//     with NoOptArgs
//     with NoKwArgs {
//   def name = "append"
//   override def filter(input: Value,
//                       args: List[Value],
//                       kwargs: Map[String, Value])(implicit ctx: Context) = {
//     val end = args(0).asInstanceOf[StringValue]
//     input match {
//       case StringValue(v) => Try(StringValue(v + end.v))
//       case v => failFragment(UnexpectedValueType(v))
//     }
//   }
// }

// case class Prepend()
//     extends Filter
//     with InputType(ValueType.String)
//     with FixedArgs(ValueType.String)
//     with NoOptArgs
//     with NoKwArgs {
//   def name = "prepend"
//   override def filter(input: Value,
//                       args: List[Value],
//                       kwargs: Map[String, Value])(implicit ctx: Context) = {
//     val start = args(0).asInstanceOf[StringValue]
//     input match {
//       case StringValue(v) => Try(StringValue(start.v + v))
//       case v => failFragment(UnexpectedValueType(v))
//     }
//   }
// }

// case class Escape()
//     extends Filter
//     with InputType(ValueType.String)
//     with NoArgs
//     with NoKwArgs {
//   def name = "escape"
//   override def filter(input: Value,
//                       args: List[Value],
//                       kwargs: Map[String, Value])(implicit ctx: Context) = {
//     input match {
//       case StringValue(v) =>
//         Try(
//           StringValue(
//             v.replace("<", "&lt;")
//               .replace(">", "&gt;")
//               .replace("\"", "&quot;")
//               .replace("&", "&amp;")))
//       case v => failFragment(UnexpectedValueType(v))
//     }
//   }
// }

// case class Remove()
//     extends Filter
//     with InputType(ValueType.String)
//     with FixedArgs(ValueType.String)
//     with NoOptArgs
//     with NoKwArgs {
//   def name = "remove"
//   override def filter(input: Value,
//                       args: List[Value],
//                       kwargs: Map[String, Value])(implicit ctx: Context) = {
//     val pattern = args(0).asInstanceOf[StringValue]
//     input match {
//       case StringValue(v) => Try(StringValue(v.replace(pattern.v, "")))
//       case v => failFragment(UnexpectedValueType(v))
//     }
//   }
// }

// case class Replace()
//     extends Filter
//     with InputType(ValueType.String)
//     with FixedArgs(ValueType.String, ValueType.String)
//     with NoOptArgs
//     with NoKwArgs {
//   def name = "replace"
//   override def filter(input: Value,
//                       args: List[Value],
//                       kwargs: Map[String, Value])(implicit ctx: Context) = {
//     val pattern = args(0).asInstanceOf[StringValue]
//     val replacement = args(1).asInstanceOf[StringValue]
//     input match {
//       case StringValue(v) =>
//         Try(StringValue(v.replace(pattern.v, replacement.v)))
//       case v => failFragment(UnexpectedValueType(v))
//     }
//   }
// }

// case class Date()
//     extends Filter
//     with InputType(ValueType.String | ValueType.Integer)
//     with FixedArgs(ValueType.String)
//     with NoOptArgs
//     with NoKwArgs {
//   import java.text.SimpleDateFormat
//   import java.util.Locale

//   def name = "date"
//   val locale = Locale.ENGLISH
//   val datePatterns: List[String] =
//     List("yyyy-MM-dd HH:mm:ss", "EEE MMM dd hh:mm:ss yyyy")

//   def liquidToJavaFormat =
//     Map(
//       // %% - Literal ``%'' character
//       '%' -> new SimpleDateFormat("%", locale),
//       // %a - The abbreviated weekday name (``Sun'')
//       'a' -> new SimpleDateFormat("EEE", locale),
//       // %A - The  full  weekday  name (``Sunday'')
//       'A' -> new SimpleDateFormat("EEEE", locale),
//       // %b - The abbreviated month name (``Jan'')
//       'b' -> new SimpleDateFormat("MMM", locale),
//       'h' -> new SimpleDateFormat("MMM", locale),
//       // %B - The  full  month  name (``January'')
//       'B' -> new SimpleDateFormat("MMMM", locale),
//       // %c - The preferred local date and time representation
//       'c' -> new SimpleDateFormat("EEE MMM dd HH:mm:ss yyyy", locale),
//       // %d - Day of the month (01..31)
//       'd' -> new SimpleDateFormat("dd", locale),
//       // %H - Hour of the day, 24-hour clock (00..23)
//       'H' -> new SimpleDateFormat("HH", locale),
//       // %I - Hour of the day, 12-hour clock (01..12)
//       'I' -> new SimpleDateFormat("hh", locale),
//       // %j - Day of the year (001..366)
//       'j' -> new SimpleDateFormat("DDD", locale),
//       // %m - Month of the year (01..12)
//       'm' -> new SimpleDateFormat("MM", locale),
//       // %M - Minute of the hour (00..59)
//       'M' -> new SimpleDateFormat("mm", locale),
//       // %p - Meridian indicator (``AM''  or  ``PM'')
//       'p' -> new SimpleDateFormat("a", locale),
//       // %S - Second of the minute (00..60)
//       'S' -> new SimpleDateFormat("ss", locale),
//       // %U - Week  number  of the current year,
//       //      starting with the first Sunday as the first
//       //      day of the first week (00..53)
//       'U' -> new SimpleDateFormat("ww", locale),
//       // %W - Week  number  of the current year,
//       //      starting with the first Monday as the first
//       //      day of the first week (00..53)
//       'W' -> new SimpleDateFormat("ww", locale),
//       // %w - Day of the week (Sunday is 0, 0..6)
//       'w' -> new SimpleDateFormat("F", locale),
//       // %x - Preferred representation for the date alone, no time
//       'x' -> new SimpleDateFormat("MM/dd/yy", locale),
//       // %X - Preferred representation for the time alone, no date
//       'X' -> new SimpleDateFormat("HH:mm:ss", locale),
//       // %y - Year without a century (00..99)
//       'y' -> new SimpleDateFormat("yy", locale),
//       // %Y - Year with century
//       'Y' -> new SimpleDateFormat("yyyy", locale),
//       // %Z - Time zone name
//       'Z' -> new SimpleDateFormat("z", locale)
//     )

//   def toSeconds(date: String): Option[Long] = {
//     datePatterns
//       .map(pattern =>
//         Try(new SimpleDateFormat(pattern, locale).parse(date).getTime / 1000L))
//       .map(_.toOption)
//       .flatten
//       .headOption
//   }

//   case class InvalidDate(filter: Filter, date: String) extends ExtensionError {
//     def description = s"`$date` is not a valid date."
//   }
//   override def filter(
//       input: Value,
//       args: List[Value],
//       kwargs: Map[String, Value])(implicit ctx: Context): Validated[Value] = {
//     val seconds: Long = input match {
//       case StringValue(v) if v == "now" => System.currentTimeMillis / 1000L
//       case StringValue(v) =>
//         toSeconds(v).getOrElse(return failFragment(InvalidDate(this, v)))
//       case IntValue(v) => v
//       case v => return failFragment(UnexpectedValueType(v)); 1L
//     }
//     val date = new java.util.Date(seconds * 1000L)

//     val format = args(0).asInstanceOf[StringValue].v
//     val calendar = java.util.Calendar.getInstance()
//     calendar.setTime(date)

//     val builder = new StringBuilder();

//     var i = 0
//     while (i < format.length) {
//       val ch = format.charAt(i);
//       if (ch == '%') {
//         i += 1

//         if (i == format.length()) {
//           builder.append("%")
//         } else {
//           val next = format.charAt(i);

//           val javaFormat = liquidToJavaFormat.get(next);

//           javaFormat match {
//             case Some(f) => builder.append(f.format(date))
//             case _ => builder.append("%").append(next);
//           }
//         }
//       } else {
//         builder.append(ch);
//       }
//       i += 1
//     }
//     Result.valid(StringValue(builder.toString))
//   }
// }

// case class Slice()
//     extends Filter
//     with InputType(ValueType.String)
//     with FixedArgs(ValueType.Integer)
//     with NoOptArgs
//     with NoKwArgs {
//   def name = "slice"
//   override def filter(input: Value,
//                       args: List[Value],
//                       kwargs: Map[String, Value])(
//       // TODO: Support negative indexes
//       implicit ctx: Context): Validated[Value] = {
//     val start = args(0).asInstanceOf[IntValue].v
//     val stop = args.lift(1).map(_.asInstanceOf[IntValue].v).getOrElse(start)
//     input match {
//       case StringValue(v) => Result.valid(StringValue(v.substring(start, stop + 1)))
//       case v => failFragment(UnexpectedValueType(v))
//     }
//   }
// }

// import shapeless._
// import shapeless.syntax.std.traversable._
// object ValueTypeables {
// }

// object FromMap {
//   implicit def caseClassFromMap[T <: HList, C](map: Map[String, Value])(implicit kw: Lazy[FromMap[T]],
//                                                                         gen: LabelledGeneric.Aux[C, T]): C = gen.from(kw.value(map))

//   implicit def kwsFromMap[K <: Symbol, H <: Value, T <: HList](implicit w: Witness.Aux[K],
//                                                                tailToKw: Lazy[FromMap[T]]
//   ): FromMap[FieldType[K, Option[H]] :: T] = (map) => {
//       val key = w.value.name
//       // TODO: Add error checking
//       val value = map.get(key).flatMap(v => v.cast[H])
//       field[K](value) :: tailToKw.value(map)
//     }

//   trait FromMap[L <: HList] {
//     def apply(map: Map[String, Value]): L
//   }
// }
}
