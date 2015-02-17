package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait MonadCombineDiscipline[F[_]] extends MonadFilterDiscipline[F] with AlternativeDiscipline[F] {

  def laws: MonadCombineLaws[F]

  def monadCombine[A, B, C](implicit
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
  ): RuleSet = new RuleSet {
    def name = "monad combine"
    def bases = Nil
    def parents = Seq(monadFilter[A, B, C], alternative[A, B, C])
    def props = Seq(
      "monad combine left distributivity" -> forAll { (fa: F[A], fa2: F[A], f: A => F[B]) =>
        laws.monadCombineLeftDistributivity(fa, fa2, f).isEqual
      }
    )
  }
}

object MonadCombineDiscipline {
  def apply[F[_]: MonadCombine]: MonadCombineDiscipline[F] = new MonadCombineDiscipline[F] {
    def laws = MonadCombineLaws[F]
  }
}
