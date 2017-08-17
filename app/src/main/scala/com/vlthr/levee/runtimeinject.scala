package com.vlthr.levee.filters
import shapeless._
import shapeless.syntax.typeable._

  /**
    * Type class supporting the injection of runtime values of type `Any` in `Coproduct`.
    *
    * Taken from an unreleased version of shapeless.
    *
    * @author Juan José Vázquez Delgado
    * @author Fabio Labella
    */
  @annotation.implicitNotFound("Implicit not found. CNil has no values, so it's impossible to convert anything to it")
trait RuntimeInject[C <: Coproduct] extends Serializable {
  def apply(x: Any): Option[C]
}

object RuntimeInject extends RuntimeInjectLowPrio {
  /**
    * Inject the receiver into a coproduct `C`, by trying to convert
    * it to each element of C.
    * Only available if the coproduct is not CNil.
    */
  def runtimeInject[C <: Coproduct](t: Any)(implicit rInj: RuntimeInject[C]): Option[C] =
    rInj(t)

  implicit def baseCaseRuntimeInject[H](
    implicit castH: Typeable[H]): RuntimeInject[H :+: CNil] =
    new RuntimeInject[H :+: CNil] {
      def apply(x: Any): Option[H :+: CNil] =
        castH.cast(x).map(v => Inl(v))
    }
}

trait RuntimeInjectLowPrio {
  implicit def inductiveCaseRuntimeInject[H, T <: Coproduct](
    implicit next: RuntimeInject[T],
    castH: Typeable[H]): RuntimeInject[H :+: T] =
    new RuntimeInject[H :+: T] {
      def apply(x: Any): Option[H :+: T] = castH.cast(x) match {
        case Some(value) => Option(Inl(value))
        case None => next(x).map(v => Inr(v))
      }
    }
}
