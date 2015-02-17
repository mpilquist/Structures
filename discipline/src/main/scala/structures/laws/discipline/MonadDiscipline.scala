package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait MonadDiscipline[F[_]] extends FlatMapDiscipline[F] with ApplicativeDiscipline[F] {

  def laws: MonadLaws[F]

  def monad[A, B, C](implicit
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
    def name = "monad"
    def bases = Nil
    def parents = Seq(flatMap[A, B, C], applicative[A, B, C])
    def props = Seq(
      "monad left identity" -> forAll { (a: A, f: A => F[B]) =>
        laws.monadLeftIdentity(a, f).isEqual
      },
      "monad right identity" -> forAll { (fa: F[A]) =>
        laws.monadRightIdentity(fa).isEqual
      }
    )
  }
}

object MonadDiscipline {
  def apply[F[_]: Monad]: MonadDiscipline[F] = new MonadDiscipline[F] {
    def laws = MonadLaws[F]
  }
}
