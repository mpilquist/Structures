package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

object UMonoidLaws {
  def apply[F[_]: UMonoid]: UMonoidLaws[F] = new UMonoidLaws[F] {
    def typeClass = UMonoid[F]
  }
}

trait UMonoidLaws[F[_]] extends USemigroupLaws[F] {

  import UMonoid.ops._, Equal.ops._

  implicit def typeClass: UMonoid[F]

  def umonoidProperties[A](implicit
    arbFA: Arbitrary[F[A]],
    eqFA: Equal[F[A]]
  ) = Seq(
    "append identity" -> forAll { (x: F[A]) =>
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


