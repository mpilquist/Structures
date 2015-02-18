package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait ApplyDiscipline[F[_]] extends FunctorDiscipline[F] {

  def laws: ApplyLaws[F]

  def apply[A: Arbitrary: Equal, B: Arbitrary, C: Arbitrary: Equal]: RuleSet = new DefaultRuleSet(
    name = "apply",
    parent = Some(functor[A, B, C]),
    props =
      "apply associative composition" -> forAll { (fa: F[A], ff: F[A => B], fg: F[B => C]) =>
        laws.applyAssociativeComposition(fa, ff, fg).isEqual
      }
  )
}

object ApplyDiscipline {
  def apply[F[_]](implicit TC: Apply[F], A: ArbitraryK[F], E: EqualK[F]): ApplyDiscipline[F] = new ApplyDiscipline[F] {
    def laws = ApplyLaws[F]
    def arbitraryKF = A
    def equalKF = E
  }
}
