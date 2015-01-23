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
      "covariant identity" -> forAll { (fa: F[A]) => Functor[F].map(fa)(x => x) == fa },
      "covariant composition" -> forAll { (fa: F[A], f: A => B, g: B => C) =>
        Functor[F].map(Functor[F].map(fa)(f))(g) == Functor[F].map(fa)(f andThen g)
      }
  )
}
