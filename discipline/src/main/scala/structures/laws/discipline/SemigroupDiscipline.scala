package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait SemigroupDiscipline[A] extends Laws {

  def laws: SemigroupLaws[A]

  def semigroup(implicit
    arbA: Arbitrary[A],
    eqA: Equal[A]
  ): RuleSet = new SimpleRuleSet(
    name = "semigroup",
    props =
      "combine associativity" -> forAll { (a: A, b: A, c: A) =>
        laws.combineAssociativity(a, b, c).isEqual
      }
  )
}

object SemigroupDiscipline {
  def apply[A: Semigroup]: SemigroupDiscipline[A] = new SemigroupDiscipline[A] {
    def laws = SemigroupLaws[A]
  }
}
