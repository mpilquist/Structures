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

  def functor[A, B, C](implicit
    arbFA: Arbitrary[F[A]],
    arbAtoB: Arbitrary[A => B],
    arbBtoA: Arbitrary[B => A],
    arbBtoC: Arbitrary[B => C],
    arbCtoB: Arbitrary[C => B]
  ): RuleSet = new DefaultRuleSet(
    name = "functor",
    parent = Some(exponential[A, B, C]),
    props =
      "covariant identity" -> forAll { (fa: F[A]) =>
        fa.map(identity) == fa
      },
      "covariant composition" -> forAll { (fa: F[A], f: A => B, g: B => C) =>
        fa.map(f).map(g) == fa.map(f andThen g)
      }
  )
}
