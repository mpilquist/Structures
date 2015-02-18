package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait MonadCombineDiscipline[F[_]] extends MonadFilterDiscipline[F] with AlternativeDiscipline[F] {

  def laws: MonadCombineLaws[F]

  def monadCombine[A: Arbitrary: Equal, B: Arbitrary: Equal, C: Arbitrary: Equal]: RuleSet = new RuleSet {
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
  def apply[F[_]](implicit TC: MonadCombine[F], A: ArbitraryK[F], E: EqualK[F]): MonadCombineDiscipline[F] = new MonadCombineDiscipline[F] {
    def laws = MonadCombineLaws[F]
    def arbitraryKF = A
    def equalKF = E
  }
}
