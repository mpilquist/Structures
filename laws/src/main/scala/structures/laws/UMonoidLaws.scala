package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object UMonoidLaws {
  def apply[F[_]: UMonoid]: UMonoidLaws[F] = new UMonoidLaws[F] {
    def typeClass = UMonoid[F]
  }
}

trait UMonoidLaws[F[_]] extends USemigroupLaws[F] {

  import UMonoid.Adapter

  implicit def typeClass: UMonoid[F]

  def umonoidProperties[A](implicit
    arbFA: Arbitrary[F[A]]
  ) = Seq(
    "append identity" -> forAll { (x: F[A]) =>
      (x |+| typeClass.empty) == x && x == (typeClass.empty |+| x)
    }
  )

  def umonoid(implicit arbFA: Arbitrary[F[Int]]): RuleSet = new RuleSet {
    def name = "umonoid"
    def bases = Nil
    def parents = Seq(usemigroup)
    def props = umonoidProperties[Int]
  }
}


