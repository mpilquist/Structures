package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait AlternativeDiscipline[F[_]] extends ApplicativeDiscipline[F] with MonoidKDiscipline[F] {

  def laws: AlternativeLaws[F]

  def alternative[A, B, C](implicit
    arbFA: Arbitrary[F[A]],
    arbA: Arbitrary[A],
    arbB: Arbitrary[B],
    arbC: Arbitrary[C],
    arbFAtoB: Arbitrary[F[A => B]],
    arbFBtoC: Arbitrary[F[B => C]],
    eqFA: Equal[F[A]],
    eqFB: Equal[F[B]],
    eqFC: Equal[F[C]]
  ): RuleSet = new RuleSet {
    def name = "alternative"
    def bases = Nil
    def parents = Seq(applicative[A, B, C], monoidK[A])
    def props = Seq(
      "alternative right distributivity" -> forAll { (fa: F[A], ff: F[A => B], fg: F[A => B]) =>
        laws.alternativeRightDistributivity(fa, ff, fg).isEqual
      },
      "alternative right absorption" -> forAll { (ff: F[A => B]) =>
        laws.alternativeRightAbsorption(ff).isEqual
      },
      "alternative left distributivity" -> forAll { (fa: F[A], fa2: F[A], f: A => B) =>
        laws.alternativeLeftDistributivity(fa, fa2, f).isEqual
      }
    )
  }
}

object AlternativeDiscipline {
  def apply[F[_]: Alternative]: AlternativeDiscipline[F] = new AlternativeDiscipline[F] {
    def laws = AlternativeLaws[F]
  }
}
