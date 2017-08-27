package com.vlthr.levee.core
import scala.util.control.NonFatal
import scala.collection.mutable.{Map => MMap}
import com.vlthr.levee.core.error._
import validation.Result
import com.vlthr.levee.filters._
import shapeless._
import shapeless.ops.traversable._
import shapeless.ops.hlist.HKernelAux

abstract trait BooleanExpr extends Expr {
  override def eval()(implicit ctx: Context): Validated[Value] =
    (left.eval() zip right.eval()) flatMap { (l, r) =>
      try {
        Result.valid(BooleanValue(op(l, r)))
      } catch {
        case NonFatal(e) => invalid(UncaughtExceptionError(e))
      }
    }
  def left: Expr
  def right: Expr
  def op(left: Value, right: Value): Boolean
}

/** Generates a range of numbers
  *
  * example: (1..5)
  */
final case class RangeExpr(left: Expr, right: Expr)(
    implicit val pctx: ParseContext)
    extends Expr {
  override def eval()(implicit ctx: Context): Validated[Value] = {
    val l = typedEval[IntValue](left)
    val r = typedEval[IntValue](right)
    (l and r) { (l, r) =>
      Value.create(
        (l.asInstanceOf[IntValue].v to r.asInstanceOf[IntValue].v).toList)
    }
  }
}

/** Checks if the value on the left contains the value on the right.
  *
  * example: myMap contains "x" */
final case class ContainsExpr(left: Expr, right: Expr)(
    implicit val pctx: ParseContext)
    extends Expr {
  override def eval()(implicit ctx: Context): Validated[Value] = {
    val l = typedEval[MapValue](left)
    val r = typedEval[StringValue](right)
    (l and r) { (l, r) =>
      BooleanValue(l.get.contains(r.get))
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

/** A value literal of any type */
final case class LiteralExpr(value: Value)(implicit val pctx: ParseContext)
    extends Expr {
  override def eval()(implicit ctx: Context) = Result.valid(value)
}

/** Use of a single variable name */
final case class VariableUseExpr(name: String)(implicit val pctx: ParseContext)
    extends Expr {
  override def eval()(implicit ctx: Context) = {
    ctx.lookup(name) match {
      case Some(value) => Result.valid(value)
      case None => invalid(UndefinedVariable(this))
    }
  }
}

/** Filter application
  *
  * example: {{ "hello" | append: " world" }} */
final case class FilterExpr(
    expr: Expr,
    filter: Filter,
    args: List[Expr],
    kwargs: Map[String, Expr])(implicit val pctx: ParseContext)
    extends Expr {
  override def eval()(implicit ctx: Context) = {
    val exprVal = expr.eval()
    val argsVal = Result.sequence(args.map(_.eval()))

    try {
      (exprVal zip argsVal) flatMap {
        case (e, as) =>
          imbueFragments(filter.apply(e, as))
      }
    } catch {
      case NonFatal(err) => invalid(UncaughtExceptionError(err))
    }
  }
}

/** List indexing
  *
  * example: list[3] */
final case class IndexExpr(indexable: Expr, key: Expr)(
    implicit val pctx: ParseContext)
    extends Expr {
  override def eval()(implicit ctx: Context) = {
    val i = typedEvalOrElse[ListValue](indexable)(v =>
      InvalidIndexable(this, indexable, v))
    val k = typedEvalOrElse[IntValue](key)(v => InvalidIndex(this, key, v))
    (i and k) { (i, k) =>
      i.get.apply(k.get)
    }

  }
}

/** Accessing the contents of a map
  *
  * example: a.b.c */
final case class DotExpr(indexable: Expr, key: String)(
    implicit val pctx: ParseContext)
    extends Expr {
  override def eval()(implicit ctx: Context) = {
    val source =
      typedEvalOrElse[MapValue](indexable)(v => InvalidMap(this, indexable, v))
    source.flatMap { source =>
      source.get.get(key) match {
        case Some(s) => valid(s)
        case None => invalid(UndefinedField(this, indexable, key))
      }
    }
  }
}
