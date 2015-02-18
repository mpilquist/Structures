package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait ExponentialDiscipline[F[_]] extends Laws {

  def laws: ExponentialLaws[F]

  implicit def arbitraryKF: ArbitraryK[F]
  implicit def equalKF: EqualK[F]

  def exponential[A: Arbitrary: Equal, B: Arbitrary, C: Arbitrary: Equal]: RuleSet = new SimpleRuleSet(
    name = "exponential",
    props =
      "exponential identity" -> forAll { (fa: F[A]) =>
        laws.exponentialIdentity(fa).isEqual
      },
      "exponential composition" -> forAll { (fa: F[A], f1: A => B, f2: B => A, g1: B => C, g2: C => B) =>
        laws.exponentialComposition(fa, f1, f2, g1, g2).isEqual
      }
  )
}

object ExponentialDiscipline {
  def apply[F[_]](implicit TC: Exponential[F], A: ArbitraryK[F], E: EqualK[F]): ExponentialDiscipline[F] = new ExponentialDiscipline[F] {
    def laws = ExponentialLaws[F]
    def arbitraryKF = A
    def equalKF = E
  }
}
