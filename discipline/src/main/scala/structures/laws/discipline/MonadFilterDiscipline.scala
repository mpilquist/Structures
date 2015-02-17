package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait MonadFilterDiscipline[F[_]] extends MonadDiscipline[F] {

  def laws: MonadFilterLaws[F]

  def monadFilter[A, B, C](implicit
    arbFA: Arbitrary[F[A]],
    arbFB: Arbitrary[F[B]],
    arbFC: Arbitrary[F[C]],
    arbA: Arbitrary[A],
    arbB: Arbitrary[B],
    arbC: Arbitrary[C],
    arbFAtoB: Arbitrary[F[A => B]],
    arbFBtoC: Arbitrary[F[B => C]],
    eqFA: Equal[F[A]],
    eqFB: Equal[F[B]],
    eqFC: Equal[F[C]]
  ): RuleSet = new DefaultRuleSet(
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
  def apply[F[_]: MonadFilter]: MonadFilterDiscipline[F] = new MonadFilterDiscipline[F] {
    def laws = MonadFilterLaws[F]
  }
}
