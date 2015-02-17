package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object SemigroupKLaws {
  def apply[F[_]: SemigroupK]: SemigroupKLaws[F] = new SemigroupKLaws[F] {
    def typeClass = SemigroupK[F]
  }
}

trait SemigroupKLaws[F[_]] extends Laws {

  import SemigroupK.ops._, Equal.ops._

  implicit def typeClass: SemigroupK[F]

  def usemigroupProperties[A](implicit
    arbFA: Arbitrary[F[A]],
    eqFA: Equal[F[A]]
  ) = Seq(
    "combine associativity" -> forAll { (x: F[A], y: F[A], z: F[A]) =>
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

