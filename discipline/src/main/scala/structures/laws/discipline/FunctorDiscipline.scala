package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait FunctorDiscipline[F[_]] extends ExponentialDiscipline[F] {

  def laws: FunctorLaws[F]

  def functor[A: Arbitrary: Equal, B: Arbitrary, C: Arbitrary: Equal]: RuleSet = new DefaultRuleSet(
    name = "functor",
    parent = Some(exponential[A, B, C]),
    props =
      "covariant identity" -> forAll { (fa: F[A]) =>
        laws.functorIdentity(fa).isEqual
      },
      "covariant composition" -> forAll { (fa: F[A], f: A => B, g: B => C) =>
        laws.functorComposition(fa, f, g).isEqual
      }
  )
}

object FunctorDiscipline {
  def apply[F[_]](implicit TC: Functor[F], A: ArbitraryK[F], E: EqualK[F]): FunctorDiscipline[F] = new FunctorDiscipline[F] {
    def laws = FunctorLaws[F]
    def arbitraryKF = A
    def equalKF = E
  }
}
