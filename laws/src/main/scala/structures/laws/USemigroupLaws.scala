package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object USemigroupLaws {
  def apply[F[_]: USemigroup]: USemigroupLaws[F] = new USemigroupLaws[F] {
    def typeClass = USemigroup[F]
  }
}

trait USemigroupLaws[F[_]] extends Laws {

  import USemigroup.ops._, Equal.ops._

  implicit def typeClass: USemigroup[F]

  def usemigroupProperties[A](implicit
    arbFA: Arbitrary[F[A]],
    eqFA: Equal[F[A]]
  ) = Seq(
    "append associativity" -> forAll { (x: F[A], y: F[A], z: F[A]) =>
      ((x |+| y) |+| z) === (x |+| (y |+| z))
    }
  )

  def usemigroup(implicit arbFA: Arbitrary[F[Int]], eqFInt: Equal[F[Int]]): RuleSet = new RuleSet {
    def name = "usemigroup"
    def bases = Nil
    def parents = Nil
    def props = usemigroupProperties[Int]
  }
}

