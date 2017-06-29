package vlthr.tee.core
import scala.collection.mutable.Map
import scala.util.{Try, Success, Failure}

abstract trait BooleanExpr extends Expr {
  override def eval()(implicit evalContext: EvalContext): Try[Value] =
    Error
      .all(left.eval, right.eval) { (l, r) =>
        Try(BooleanValue(op(l, r))).recoverWith(_ =>
          Error.incomparableValues(this, l, r))
      }
      .flatten
  def left: Expr
  def right: Expr
  def op(left: Value, right: Value): Boolean
}

final case class AndExpr(left: Expr, right: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends BooleanExpr {
  override def op(l: Value, r: Value): Boolean = l.truthy && r.truthy
}

final case class OrExpr(left: Expr, right: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends BooleanExpr {
  override def op(left: Value, right: Value): Boolean =
    left.truthy || right.truthy
}

final case class EqExpr(left: Expr, right: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends BooleanExpr {
  override def op(left: Value, right: Value): Boolean = left == right
}

final case class NEqExpr(left: Expr, right: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends BooleanExpr {
  override def op(left: Value, right: Value): Boolean = left != right
}

final case class LEqExpr(left: Expr, right: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends BooleanExpr {
  override def op(left: Value, right: Value): Boolean = left <= right
}

final case class LtExpr(left: Expr, right: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends BooleanExpr {
  override def op(left: Value, right: Value): Boolean = left < right
}

final case class GEqExpr(left: Expr, right: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends BooleanExpr {
  override def op(left: Value, right: Value): Boolean = left >= right
}

final case class GtExpr(left: Expr, right: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends BooleanExpr {
  override def op(left: Value, right: Value): Boolean = left > right
}

final case class LiteralExpr(value: Value)(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = Success(value)
}

final case class VariableUseExpr(name: String)(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = {
    evalContext.lookup(name) match {
      case Some(value) => Success(value)
      case None => Error.undefinedVariable(this)
    }
  }
}

final case class FilterExpr(expr: Expr, filter: Filter, args: List[Expr])(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) =
    Error.all(expr.eval) { result =>
      filter.apply(result)
    }
}

final case class IndexExpr(indexable: Expr, key: Expr)(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = {
    val i: Try[List[Value]] = indexable.eval.flatMap {
      case ListValue(s) => Success(s)
      case x => Error.invalidIndexable(this, indexable)
    }
    val k: Try[Int] = key.eval.flatMap {
      case IntValue(i) => Success(i)
      case x => Error.invalidIndex(this, key)
    }
    Error.all(i, k) { (i, k) =>
      i(k)
    }

  }
}

final case class DotExpr(indexable: Expr, key: String)(
    implicit val sourcePosition: SourcePosition)
    extends Expr {
  override def eval()(implicit evalContext: EvalContext) = {
    val source: Try[Map[String, Value]] = indexable.eval.flatMap {
      case MapValue(m) => Success(m)
      case x => Error.invalidMap(this, indexable)
    }
    Error
      .all(source) { source =>
        source.get(key) match {
          case Some(s) => Success(s)
          case None => Error.undefinedField(this, indexable, key)
        }
      }
      .flatten
  }
}
