package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait MonoidKDiscipline[F[_]] extends SemigroupKDiscipline[F] {

  def laws: MonoidKLaws[F]

  def monoidK[A](implicit
    arbFA: Arbitrary[F[A]],
    eqFA: Equal[F[A]]
  ): RuleSet = new DefaultRuleSet(
    name = "monoidK",
    parent = Some(semigroupK),
    props =
      "combine left identity" -> forAll { (a: F[A]) =>
        laws.combineLeftIdentity(a).isEqual
      },
      "combine right identity" -> forAll { (a: F[A]) =>
        laws.combineRightIdentity(a).isEqual
      }
  )
}

object MonoidKDiscipline {
  def apply[F[_]: MonoidK]: MonoidKDiscipline[F] = new MonoidKDiscipline[F] {
    def laws = MonoidKLaws[F]
  }
}
