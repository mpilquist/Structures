package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait MonadDiscipline[F[_]] extends FlatMapDiscipline[F] with ApplicativeDiscipline[F] {

  def laws: MonadLaws[F]

  def monad[A: Arbitrary: Equal, B: Arbitrary: Equal, C: Arbitrary: Equal]: RuleSet = new RuleSet {
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
  def apply[F[_]](implicit TC: Monad[F], A: ArbitraryK[F], E: EqualK[F]): MonadDiscipline[F] = new MonadDiscipline[F] {
    def laws = MonadLaws[F]
    def arbitraryKF = A
    def equalKF = E
  }
}
