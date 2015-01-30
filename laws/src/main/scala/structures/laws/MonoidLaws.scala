package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object MonoidLaws {
  def apply[A: Monoid]: MonoidLaws[A] = new MonoidLaws[A] {
    def typeClass = Monoid[A]
  }
}

trait MonoidLaws[A] extends SemigroupLaws[A] {

  import Monoid.Adapter

  implicit def typeClass: Monoid[A]

  def monoidProperties(implicit
    arbA: Arbitrary[A]
  ) = Seq(
    "append identity" -> forAll { (x: A) =>
      (x |+| typeClass.empty) == x && x == (typeClass.empty |+| x)
    }
  )

  def monoid(implicit arbA: Arbitrary[A]): RuleSet = new RuleSet {
    def name = "monoid"
    def bases = Nil
    def parents = Seq(semigroup)
    def props = monoidProperties
  }
}



