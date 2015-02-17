package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait MonoidDiscipline[A] extends SemigroupDiscipline[A] {

  def laws: MonoidLaws[A]

  def monoid(implicit
    arbA: Arbitrary[A],
    eqA: Equal[A]
  ): RuleSet = new DefaultRuleSet(
    name = "monoid",
    parent = Some(semigroup),
    props =
      "combine left identity" -> forAll { (a: A) =>
        laws.combineLeftIdentity(a).isEqual
      },
      "combine right identity" -> forAll { (a: A) =>
        laws.combineRightIdentity(a).isEqual
      }
  )
}

object MonoidDiscipline {
  def apply[A: Monoid]: MonoidDiscipline[A] = new MonoidDiscipline[A] {
    def laws = MonoidLaws[A]
  }
}
