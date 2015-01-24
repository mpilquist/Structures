package fundamentum
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

  import Functor.Adapter

  implicit def typeClass: Functor[F]

  def functorProperties[A, B, C](implicit
    arbFA: Arbitrary[F[A]],
    arbAtoB: Arbitrary[A => B],
    arbBtoC: Arbitrary[B => C]
  ) = Seq(
    "covariant identity" -> forAll { (fa: F[A]) =>
      fa.map(identity) == fa
    },
    "covariant composition" -> forAll { (fa: F[A], f: A => B, g: B => C) =>
      fa.map(f).map(g) == fa.map(f andThen g)
    }
  )

  def functor(implicit arbFInt: Arbitrary[F[Int]]): RuleSet = new RuleSet {
    def name = "functor"
    def bases = Nil
    def parents = Seq(exponential)
    def props = functorProperties[Int, String, Long]
  }
}
