package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait SemigroupKDiscipline[F[_]] extends Laws {

  def laws: SemigroupKLaws[F]

  def semigroupK[A](implicit
    arbFA: Arbitrary[F[A]],
    eqFA: Equal[F[A]]
  ): RuleSet = new SimpleRuleSet(
    name = "semigroupK",
    props =
      "combine associativity" -> forAll { (a: F[A], b: F[A], c: F[A]) =>
        laws.combineAssociativity(a, b, c).isEqual
      }
  )
}

object SemigroupKDiscipline {
  def apply[F[_]: SemigroupK]: SemigroupKDiscipline[F] = new SemigroupKDiscipline[F] {
    def laws = SemigroupKLaws[F]
  }
}
