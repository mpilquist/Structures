package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait MonadFilterDiscipline[F[_]] extends MonadDiscipline[F] {

  def laws: MonadFilterLaws[F]

  def monadFilter[A: Arbitrary: Equal, B: Arbitrary: Equal, C: Arbitrary: Equal]: RuleSet = new DefaultRuleSet(
    name = "monad",
    parent = Some(monad[A, B, C]),
    props =
      "monad filter left distributivity" -> forAll { (f: A => F[B]) =>
        laws.monadFilterLeftDistributivity(f).isEqual
      },
      "monad filter right distributivity" -> forAll { (fa: F[A]) =>
        laws.monadFilterRightDistributivity(fa).isEqual
      }
  )
}

object MonadFilterDiscipline {
  def apply[F[_]](implicit TC: MonadFilter[F], A: ArbitraryK[F], E: EqualK[F]): MonadFilterDiscipline[F] = new MonadFilterDiscipline[F] {
    def laws = MonadFilterLaws[F]
    def arbitraryKF = A
    def equalKF = E
  }
}
