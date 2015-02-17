package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

object MonoidKLaws {
  def apply[F[_]: MonoidK]: MonoidKLaws[F] = new MonoidKLaws[F] {
    def typeClass = MonoidK[F]
  }
}

trait MonoidKLaws[F[_]] extends SemigroupKLaws[F] {

  import MonoidK.ops._, Equal.ops._

  implicit def typeClass: MonoidK[F]

  def umonoidProperties[A](implicit
    arbFA: Arbitrary[F[A]],
    eqFA: Equal[F[A]]
  ) = Seq(
    "combine identity" -> forAll { (x: F[A]) =>
      (x |+| typeClass.empty) === x && x === (typeClass.empty |+| x)
    }
  )

  def umonoid(implicit arbFA: Arbitrary[F[Int]], eqFInt: Equal[F[Int]]): RuleSet = new RuleSet {
    def name = "umonoid"
    def bases = Nil
    def parents = Seq(usemigroup)
    def props = umonoidProperties[Int]
  }
}


