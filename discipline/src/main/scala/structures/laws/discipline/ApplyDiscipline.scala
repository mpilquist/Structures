package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait ApplyDiscipline[F[_]] extends FunctorDiscipline[F] {

  def laws: ApplyLaws[F]

  def apply[A, B, C](implicit
    arbFA: Arbitrary[F[A]],
    arbA: Arbitrary[A],
    arbB: Arbitrary[B],
    arbC: Arbitrary[C],
    arbFAtoB: Arbitrary[F[A => B]],
    arbFBtoC: Arbitrary[F[B => C]],
    eqFA: Equal[F[A]],
    eqFC: Equal[F[C]]
  ): RuleSet = new DefaultRuleSet(
    name = "apply",
    parent = Some(functor[A, B, C]),
    props =
      "apply associative composition" -> forAll { (fa: F[A], ff: F[A => B], fg: F[B => C]) =>
        laws.applyAssociativeComposition(fa, ff, fg).isEqual
      }
  )
}

object ApplyDiscipline {
  def apply[F[_]: Apply]: ApplyDiscipline[F] = new ApplyDiscipline[F] {
    def laws = ApplyLaws[F]
  }
}
