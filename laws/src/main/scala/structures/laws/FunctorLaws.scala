package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object FunctorLaws {
  def apply[F[_]: Functor]: FunctorLaws[F] = new FunctorLaws[F] {
    def typeClass = Functor[F]
  }
}

trait FunctorLaws[F[_]] extends ExponentialLaws[F] {

  import Functor.ops._, Equal.ops._

  implicit def typeClass: Functor[F]

  def functorProperties[A, B, C](implicit
    arbFA: Arbitrary[F[A]],
    arbAtoB: Arbitrary[A => B],
    arbBtoC: Arbitrary[B => C],
    eqFA: Equal[F[A]],
    eqFC: Equal[F[C]]
  ) = Seq(
    "covariant identity" -> forAll { (fa: F[A]) =>
      fa.map(identity) === fa
    },
    "covariant composition" -> forAll { (fa: F[A], f: A => B, g: B => C) =>
      fa.map(f).map(g) === fa.map(f andThen g)
    }
  )

  def functor(implicit arbFInt: Arbitrary[F[Int]], eqFInt: Equal[F[Int]], eqFLong: Equal[F[Long]]): RuleSet = new RuleSet {
    def name = "functor"
    def bases = Nil
    def parents = Seq(exponential)
    def props = functorProperties[Int, String, Long]
  }
}
