package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait ContravariantDiscipline[F[_]] extends ExponentialDiscipline[F] {

  def laws: ContravariantLaws[F]

  def contravariant[A, B, C](implicit
    arbFA: Arbitrary[F[A]],
    arbA: Arbitrary[A],
    arbB: Arbitrary[B],
    arbC: Arbitrary[C],
    eqFA: Equal[F[A]],
    eqFC: Equal[F[C]]
  ): RuleSet = new DefaultRuleSet(
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
  def apply[F[_]: Contravariant]: ContravariantDiscipline[F] = new ContravariantDiscipline[F] {
    def laws = ContravariantLaws[F]
  }
}
