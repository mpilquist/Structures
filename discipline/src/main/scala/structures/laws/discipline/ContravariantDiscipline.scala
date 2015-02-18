package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait ContravariantDiscipline[F[_]] extends ExponentialDiscipline[F] {

  def laws: ContravariantLaws[F]

  def contravariant[A: Arbitrary: Equal, B: Arbitrary, C: Arbitrary: Equal]: RuleSet = new DefaultRuleSet(
    name = "contravariant",
    parent = Some(exponential[A, B, C]),
    props =
      "contravariant identity" -> forAll { (fa: F[A]) =>
        laws.contravariantIdentity(fa).isEqual
      },
      "contravariant composition" -> forAll { (fa: F[A], f: B => A, g: C => B) =>
        laws.contravariantComposition(fa, f, g).isEqual
      }
  )
}

object ContravariantDiscipline {
  def apply[F[_]](implicit TC: Contravariant[F], A: ArbitraryK[F], E: EqualK[F]): ContravariantDiscipline[F] = new ContravariantDiscipline[F] {
    def laws = ContravariantLaws[F]
    def arbitraryKF = A
    def equalKF = E
  }
}
