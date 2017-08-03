package vlthr.tee.core
import scala.util.control.NonFatal
import scala.collection.mutable.{Map => MMap}
import scala.util.{Try, Success, Failure}
import vlthr.tee.core.Error._
import vlthr.tee.filters._

abstract trait BooleanExpr extends Expr {
  override def eval()(implicit ctx: Context): Try[Value] =
    Error
      .all(left.eval(), right.eval()) { (l, r) =>
        Try(BooleanValue(op(l, r))).recoverWith(_ =>
          fail(IncomparableValues(this, l, r)))
      }
      .flatten
  def left: Expr
  def right: Expr
  def op(left: Value, right: Value): Boolean
}

final case class RangeExpr(left: Expr, right: Expr)(
    implicit val pctx: ParseContext)
    extends Expr {
  override def eval()(implicit ctx: Context): Try[Value] = {
    val l = left.eval().flatMap {
      case v: IntValue => Success(v)
      case v =>
        fail(
          UnexpectedValueType(v, expected = Some(ValueType.Integer))
            .imbue(pctx))
    }
    val r = right.eval().flatMap {
      case v: IntValue => Success(v)
      case v =>
        fail(
          UnexpectedValueType(v, expected = Some(ValueType.Integer))
            .imbue(pctx))
    }
    Error
      .all(l, r) { (l, r) =>
        Value.create(
          (l.asInstanceOf[IntValue].v to r.asInstanceOf[IntValue].v).toList)
      }
  }
}

final case class ContainsExpr(left: Expr, right: Expr)(
    implicit val pctx: ParseContext)
    extends Expr {
  override def eval()(implicit ctx: Context): Try[Value] = {
    val l = left.eval().flatMap {
      case MapValue(m) => Success(m)
      case v =>
        fail(
          UnexpectedValueType(v, expected = Some(ValueType.Map)).imbue(pctx))
    }
    val r = right.eval().flatMap {
      case StringValue(s) => Success(s)
      case v =>
        fail(
          UnexpectedValueType(v, expected = Some(ValueType.String))
            .imbue(pctx))
    }
    Error
      .all(l, r) { (l, r) =>
        BooleanValue(
          l.asInstanceOf[Map[String, Value]].contains(r.asInstanceOf[String]))
      }
  }
}

final case class AndExpr(left: Expr, right: Expr)(
    implicit val pctx: ParseContext)
    extends BooleanExpr {
  override def op(l: Value, r: Value): Boolean = l.truthy && r.truthy
}

final case class OrExpr(left: Expr, right: Expr)(
    implicit val pctx: ParseContext)
    extends BooleanExpr {
  override def op(left: Value, right: Value): Boolean =
    left.truthy || right.truthy
}

final case class EqExpr(left: Expr, right: Expr)(
    implicit val pctx: ParseContext)
    extends BooleanExpr {
  override def op(left: Value, right: Value): Boolean = left == right
}

final case class NEqExpr(left: Expr, right: Expr)(
    implicit val pctx: ParseContext)
    extends BooleanExpr {
  override def op(left: Value, right: Value): Boolean = left != right
}

final case class LEqExpr(left: Expr, right: Expr)(
    implicit val pctx: ParseContext)
    extends BooleanExpr {
  override def op(left: Value, right: Value): Boolean = left <= right
}

final case class LtExpr(left: Expr, right: Expr)(
    implicit val pctx: ParseContext)
    extends BooleanExpr {
  override def op(left: Value, right: Value): Boolean = left < right
}

final case class GEqExpr(left: Expr, right: Expr)(
    implicit val pctx: ParseContext)
    extends BooleanExpr {
  override def op(left: Value, right: Value): Boolean = left >= right
}

final case class GtExpr(left: Expr, right: Expr)(
    implicit val pctx: ParseContext)
    extends BooleanExpr {
  override def op(left: Value, right: Value): Boolean = left > right
}

final case class LiteralExpr(value: Value)(implicit val pctx: ParseContext)
    extends Expr {
  override def eval()(implicit ctx: Context) = Success(value)
}

final case class VariableUseExpr(name: String)(implicit val pctx: ParseContext)
    extends Expr {
  override def eval()(implicit ctx: Context) = {
    ctx.lookup(name) match {
      case Some(value) => Success(value)
      case None => fail(UndefinedVariable(this))
    }
  }
}

final case class FilterExpr(
    expr: Expr,
    filter: Filter,
    args: List[Expr],
    kwargs: Map[String, Expr])(implicit val pctx: ParseContext)
    extends Expr {
  override def eval()(implicit ctx: Context) = {
    val exprVal = expr.eval()
    val argsVal = args.map(_.eval())
    val kwArgsVal = kwargs.map { case (k, v) => (k, v.eval()) }
    val kwEvals = kwArgsVal.values
    val possibleErrors = argsVal ++ kwEvals :+ exprVal
    Error.all(possibleErrors) {
      val expr = exprVal.get
      val args = argsVal.map(_.get)
      val kwargs = kwArgsVal.map { case (k, v) => (k, v.get) }
      Try {
        filter
          .typeCheck(expr, args)
          .flatMap(_ => filter.filter(expr, args, kwargs))
          .recoverWith {
            case LiquidFailure(errors) =>
              fail(Error.imbueFragments(errors): _*)
            case NonFatal(e) => fail(UncaughtExceptionError(e))
            case e => fail(UncaughtExceptionError(e))
          }
      }.flatten
    }
  }
}

final case class IndexExpr(indexable: Expr, key: Expr)(
    implicit val pctx: ParseContext)
    extends Expr {
  override def eval()(implicit ctx: Context) = {
    val i: Try[List[Value]] = indexable.eval().flatMap {
      case ListValue(s) => Success(s)
      case x => fail(InvalidIndexable(this, indexable, x))
    }
    val k: Try[Int] = key.eval().flatMap {
      case IntValue(i) => Success(i)
      case x => fail(InvalidIndex(this, key, x))
    }
    Error.all(i, k) { (i, k) =>
      i(k)
    }

  }
}

final case class DotExpr(indexable: Expr, key: String)(
    implicit val pctx: ParseContext)
    extends Expr {
  override def eval()(implicit ctx: Context) = {
    val source: Try[Map[String, Value]] = indexable.eval().flatMap {
      case MapValue(m) => Success(m)
      case x => fail(InvalidMap(this, indexable, x))
    }
    Error
      .all(source) { source =>
        source.get(key) match {
          case Some(s) => Success(s)
          case None => fail(UndefinedField(this, indexable, key))
        }
      }
      .flatten
  }
}
