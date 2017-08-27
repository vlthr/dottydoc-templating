package com.vlthr.levee
import scala.util.{Try, Success, Failure}
import scala.util.control.NonFatal
import com.vlthr.levee.parser.LeveeParser
import com.vlthr.levee.core._
import com.vlthr.levee.util._
import com.vlthr.levee.core.error._
import validation.Result
import scala.collection.mutable.{Map => MMap, Set => MSet}
import com.fasterxml.jackson.databind.ObjectMapper
import shapeless._
import shapeless.ops.hlist.HKernelAux
import shapeless.ops.traversable._
import shapeless.syntax.typeable._
import shapeless.ops.coproduct._
import shapeless.syntax.inject._
import com.vlthr.levee.core.Value._

package object filters {
  abstract trait Filter extends Extension {
    type Input
    type Args <: HList
    type OptArgs <: HList
    def name: String
    def filter(input: Input, args: Args, optArgs: OptArgs)(
        implicit ctx: Context): ValidatedFragment[Value]
    def extensionType = "filter"
    def apply(input: Value, allArgs: List[Value])(
        implicit ctx: Context): ValidatedFragment[Value]
  }

  object Filter {
    def intLen[T <: HList](implicit ker: HKernelAux[T]): Int = ker().length

    def noOptArgs[I, A <: HList](s: String)(
        f: (Context, Filter, I, A, HNil) => ValidatedFragment[Value])(
        implicit ftArgs: FromTraversable[A],
        itype: Typeable[I],
        hkArgs: HKernelAux[A]) = {
      def matchInput(filter: Filter, input: Value) =
        Result.fromOption(input.cast[I], InvalidInput(filter, input))
      def matchArgs(filter: Filter, args: List[Value]) = {
        val a =
          if (args.size < intLen[A]) failure(TooFewArgs(filter, args))
          else if (args.size > intLen[A]) failure(TooManyArgs(filter, args))
          else Result.fromOption(ftArgs(args), InvalidArgs(filter, args))
        a zip success(HNil)
      }

      base[I, A, HNil](s, matchInput, matchArgs)(f)
    }

    def apply[I, A <: HList, O <: HList](s: String)(
        f: (Context, Filter, I, A, O) => ValidatedFragment[Value])(
        implicit ftArgs: FromTraversable[A],
        ftOpt: FromTraversable[O],
        itype: Typeable[I],
        hkArgs: HKernelAux[A],
        hkOpt: HKernelAux[O]) = {
      def matchInput(filter: Filter, input: Value) =
        Result.fromOption(input.cast[I], InvalidInput(filter, input))
      def matchArgs(filter: Filter, allArgs: List[Value]) = {
        val (args, optArgs) = allArgs.splitAt(intLen[A])
        val maxNrOpts = intLen[O]
        val a =
          if (args.size < intLen[A]) failure(TooFewArgs(filter, args))
          else if (args.size > (intLen[A] + intLen[O]))
            failure(TooManyArgs(filter, args))
          else Result.fromOption(ftArgs(args), InvalidArgs(filter, args))
        val fixedOptArgs = optArgs.map(v => Some(v)) ++ List.fill(
          maxNrOpts - optArgs.size)(None)
        val o =
          Result.fromOption(ftOpt(fixedOptArgs), InvalidOptArgs(filter, args))
        a zip o
      }

      Filter.base[I, A, O](s, matchInput, matchArgs)(f)
    }

    def multiNoOptArgs[I <: Coproduct, A <: HList](s: String)(
        f: (Context, Filter, I, A, HNil) => ValidatedFragment[Value])(
        implicit ftArgs: FromTraversable[A],
        itype: Typeable[I],
        rinj: RuntimeInject[I],
        hkArgs: HKernelAux[A]) = {
      def matchInput(filter: Filter, input: Value) =
        Result.fromOption(Coproduct.runtimeInject[I](input),
                          InvalidInput(filter, input))
      def matchArgs(filter: Filter, allArgs: List[Value]) = {
        val a =
          if (allArgs.size < intLen[A]) failure(TooFewArgs(filter, allArgs))
          else if (allArgs.size > intLen[A])
            failure(TooManyArgs(filter, allArgs))
          else Result.fromOption(ftArgs(allArgs), InvalidArgs(filter, allArgs))
        a zip success(HNil)
      }
      base[I, A, HNil](s, matchInput, matchArgs)(f)
    }

    def multi[I <: Coproduct, A <: HList, O <: HList](s: String)(
        f: (Context, Filter, I, A, O) => ValidatedFragment[Value])(
        implicit ftArgs: FromTraversable[A],
        ftOpt: FromTraversable[O],
        itype: Typeable[I],
        rinj: RuntimeInject[I],
        hkArgs: HKernelAux[A],
        hkOpt: HKernelAux[O]) = {
      def matchInput(filter: Filter, input: Value) =
        Result.fromOption(Coproduct.runtimeInject[I](input),
                          InvalidInput(filter, input))
      def matchArgs(filter: Filter, allArgs: List[Value]) = {
        val (args, optArgs) = allArgs.splitAt(intLen[A])
        val maxNrOpts = intLen[O]
        val a =
          if (allArgs.size < intLen[A]) failure(TooFewArgs(filter, args))
          else if (allArgs.size > (intLen[A] + intLen[O]))
            failure(TooManyArgs(filter, args))
          else Result.fromOption(ftArgs(args), InvalidArgs(filter, args))
        val fixedOptArgs = optArgs.map(v => Some(v)) ++ List.fill(
          maxNrOpts - optArgs.size)(None)
        val o =
          Result.fromOption(ftOpt(fixedOptArgs), InvalidOptArgs(filter, args))
        a zip o
      }
      base[I, A, O](s, matchInput, matchArgs)(f)
    }
    type MatchInput[T] = (Filter, Value) => ValidatedFragment[T]
    type MatchArgs[A, O] = (Filter, List[Value]) => ValidatedFragment[(A, O)]
    def base[I, A <: HList, O <: HList](s: String,
                                        matchInput: MatchInput[I],
                                        matchArgs: MatchArgs[A, O])(
        f: (Context, Filter, I, A, O) => ValidatedFragment[Value]): Filter =
      new Filter {
        type Input = I
        type Args = A
        type OptArgs = O
        def name = s
        def filter(input: Input, args: Args, optArgs: OptArgs)(
            implicit ctx: Context): ValidatedFragment[Value] =
          f(ctx, this, input, args, optArgs)
        def apply(input: Value, allArgs: List[Value])(
            implicit ctx: Context): ValidatedFragment[Value] = {
          val i = matchInput(this, input)
          val as = matchArgs(this, allArgs)
          (i zip as).flatMap { (i, allArgs) =>
            val (args, optArgs) = allArgs
            filter(i, args, optArgs)
          }
        }
      }

    def byName(s: String): Option[Filter] = registry.get(s)
    val registry: MMap[String, Filter] = MMap(
      "split" -> split,
      "json" -> json,
      "date" -> Date(),
      "slice" -> slice,
      "join" -> join,
      "size" -> size,
      "first" -> first,
      "last" -> last,
      "prepend" -> prepend,
      "append" -> append,
      "capitalize" -> capitalize,
      "downcase" -> downcase,
      "upcase" -> upcase,
      "escape" -> escape,
      "remove" -> remove,
      "replace" -> replace,
      "reverse" -> reverse
    )
  }

  /** Represents an empty set of arguments or optarguments.
    */
  type Empty = HNil

  object Empty {
    import scala.collection.GenTraversable
    import shapeless.ops.hlist.HNilHKernel
    implicit def ftEmpty: FromTraversable[Empty] = new FromTraversable[Empty] {
      type Out = Nothing
      def apply(l: GenTraversable[_]): Option[Out] = None
    }
    implicit def hkEmpty: HKernelAux[Empty] = new HKernelAux[Empty] {
      type Out = HNilHKernel
      def apply() = HNilHKernel
    }
  }

  def unknownFilter(n: String) = new Filter {
    def name: String = n
    def filter(input: Input, args: Args, optArgs: OptArgs)(
        implicit ctx: Context): ValidatedFragment[Value] = ???
    override def apply(input: Value, allArgs: List[Value])(
        implicit ctx: Context): ValidatedFragment[Value] = {
      failure(UnknownFilterName(name))
    }
  }

  val split = Filter[StringValue, StringValue :: HNil, Empty]("split") {
    (ctx, filter, input, args, optArgs) =>
      val pattern = args.head.get
      val stringToSplit = input.get
      success(Value.create(stringToSplit.split(pattern).toList))
  }

  val json = Filter.noOptArgs[ListValue, Empty]("json") {
    (ctx, filter, input, args, optArgs) =>
      success(
        StringValue(new ObjectMapper()
          .writeValueAsString(Util.asJava(input))))
  }

  val size =
    Filter.multiNoOptArgs[ListValue :+: StringValue :+: CNil, Empty]("size") {
      (ctx, filter, input, args, optArgs) =>
        input match {
          case Inl(list) => success(IntValue(list.get.size))
          case Inr(Inl(string)) => success(IntValue(string.get.size))
          // case Inr(Inr(_)) => abort()
          case _ => abort()
        }
    }

  val first =
    Filter.multiNoOptArgs[ListValue :+: StringValue :+: CNil, Empty]("first") {
      (ctx, filter, input, args, optArgs) =>
        input match {
          case Inl(l) => success(l.get.head)
          case Inr(Inl(s)) => success(StringValue("" + s.get.head))
          // case Inr(Inr(_)) => abort()
          case _ => abort()
        }
    }

  val last =
    Filter.multiNoOptArgs[ListValue :+: StringValue :+: CNil, Empty]("last") {
      (ctx, filter, input, args, optArgs) =>
        input match {
          case Inl(list) => success(list.get.last)
          case Inr(Inl(string)) => success(StringValue("" + string.get.last))
          // case Inr(Inr(_)) => abort()
          case _ => abort()
        }
    }

  val reverse =
    Filter.multiNoOptArgs[ListValue :+: StringValue :+: CNil, Empty]("reverse") {
      (ctx, filter, input, args, optArgs) =>
        input match {
          case Inl(list) => success(ListValue(list.get.reverse))
          case Inr(Inl(string)) => success(StringValue(string.get.reverse))
          // case Inr(Inr(_)) => abort()
          case _ => abort()
        }
    }

  val join = Filter[ListValue, StringValue :: HNil, Empty]("join") {
    (ctx, filter, input, args, optArgs) =>
      val delim = args.head.get
      val elems = Result.sequence(input.get.map(_.render()(ctx)))
      elems.map(es => StringValue(es.mkString(delim)))
  }

  val capitalize = Filter.noOptArgs[StringValue, Empty]("capitalize") {
    (ctx, filter, input, args, optArgs) =>
      val i = input.get
      success(StringValue(Character.toUpperCase(i(0)) + i.substring(1)))
  }

  val downcase = Filter.noOptArgs[StringValue, Empty]("downcase") {
    (ctx, filter, input, args, optArgs) =>
      success(
        StringValue(input.get.map(c => Character.toLowerCase(c)).mkString))
  }

  val upcase = Filter.noOptArgs[StringValue, Empty]("upcase") {
    (ctx, filter, input, args, optArgs) =>
      success(
        StringValue(input.get.map(c => Character.toUpperCase(c)).mkString))
  }

  val prepend = Filter[StringValue, StringValue :: HNil, Empty]("prepend") {
    (ctx, filter, input, args, optArgs) =>
      val start = args.head.get
      success(StringValue(start + input.get))
  }

  val append = Filter[StringValue, StringValue :: HNil, Empty]("append") {
    (ctx, filter, input, args, optArgs) =>
      val start = args.head.get
      success(StringValue(input.get + start))
  }

  val escape = Filter.noOptArgs[StringValue, Empty]("escape") {
    (ctx, filter, input, args, optArgs) =>
      success(
        StringValue(
          input.get
            .replace("<", "&lt;")
            .replace(">", "&gt;")
            .replace("\"", "&quot;")
            .replace("&", "&amp;")))
  }

  val remove = Filter[StringValue, StringValue :: HNil, Empty]("remove") {
    (ctx, filter, input, args, optArgs) =>
      val pattern = args.head.get
      success(StringValue(input.get.replace(pattern, "")))
  }

  val replace =
    Filter[StringValue, StringValue :: StringValue :: HNil, Empty]("replace") {
      (ctx, filter, input, args, optArgs) =>
        val pattern = args.head.get
        val replacement = args.tail.head.get
        success(StringValue(input.get.replace(pattern, replacement)))
    }

  /** Collects logic relating to the date filter
    *
    * Adapted from Bart Kiers's date implementation in Liqp
    */
  object Date {
    import java.text.SimpleDateFormat
    import java.util.Locale

    case class InvalidDate(filter: Filter, date: String)
        extends ExtensionError {
      def description = s"`$date` is not a valid date."
    }

    def toSeconds(date: String): Option[Long] = {
      datePatterns
        .map(pattern =>
          Try(
            new SimpleDateFormat(pattern, locale).parse(date).getTime / 1000L))
        .map(_.toOption)
        .flatten
        .headOption
    }

    val locale = Locale.ENGLISH
    val datePatterns: List[String] =
      List("yyyy-MM-dd HH:mm:ss", "EEE MMM dd hh:mm:ss yyyy")

    def liquidToJavaFormat =
      Map(
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
        'Z' -> new SimpleDateFormat("z", locale)
      )

    val date =
      Filter
        .multi[IntValue :+: StringValue :+: CNil, StringValue :: HNil, Empty](
          "date") { (ctx, filter, input, args, optArgs) =>
          val seconds: ValidatedFragment[Long] = input match {
            case Inl(int) =>
              success(int.get)
            case Inr(Inl(string)) if string.get == "now" =>
              success(System.currentTimeMillis / 1000L)
            case Inr(Inl(string)) =>
              toSeconds(string.get)
                .map(success)
                .getOrElse(failure(InvalidDate(filter, string.get)))
            case _ => abort()
          }
          val date = seconds.map(s => new java.util.Date(s * 1000L))

          val format = args.head.get

          date.map { date =>
            val calendar = java.util.Calendar.getInstance()
            calendar.setTime(date)

            val builder = new StringBuilder();

            var i = 0
            while (i < format.length) {
              val ch = format.charAt(i);
              if (ch == '%') {
                i += 1

                if (i == format.length()) {
                  builder.append("%")
                } else {
                  val next = format.charAt(i);

                  val javaFormat = liquidToJavaFormat.get(next);

                  javaFormat match {
                    case Some(f) => builder.append(f.format(date))
                    case _ => builder.append("%").append(next);
                  }
                }
              } else {
                builder.append(ch);
              }
              i += 1
            }
            StringValue(builder.toString)
          }
        }
    def apply() = date
  }

  val slice =
    Filter[StringValue, IntValue :: HNil, Option[IntValue] :: HNil]("slice") {
      (ctx, filter, input, args, optArgs) =>
        // TODO: Support negative indexes
        val start = args.head.get
        val stop = optArgs.head.map(_.get).getOrElse(start)
        success(StringValue(input.get.substring(start, stop + 1)))
    }
}
