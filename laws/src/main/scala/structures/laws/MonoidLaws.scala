package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

object MonoidLaws {
  def apply[A: Monoid]: MonoidLaws[A] = new MonoidLaws[A] {
    def typeClass = Monoid[A]
  }
}

trait MonoidLaws[A] extends SemigroupLaws[A] {

  import Monoid.ops._, Equal.ops._

  implicit def typeClass: Monoid[A]

  def monoidProperties(implicit
    arbA: Arbitrary[A],
    eqA: Equal[A]
  ) = Seq(
    "append identity" -> forAll { (x: A) =>
      (x |+| typeClass.empty) === x && x === (typeClass.empty |+| x)
    }
  )

  def monoid(implicit arbA: Arbitrary[A], eqA: Equal[A]): RuleSet = new RuleSet {
    def name = "monoid"
    def bases = Nil
    def parents = Seq(semigroup)
    def props = monoidProperties
  }
}

