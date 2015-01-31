package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object ContravariantLaws {
  def apply[F[_]: Contravariant]: ContravariantLaws[F] = new ContravariantLaws[F] {
    def typeClass = Contravariant[F]
  }
}

trait ContravariantLaws[F[_]] extends ExponentialLaws[F] {

  import Contravariant.Adapter

  implicit def typeClass: Contravariant[F]

  def contravariantProperties[A, B, C](implicit
    arbFA: Arbitrary[F[A]],
    arbBtoA: Arbitrary[B => A],
    arbCtoB: Arbitrary[C => B]
  ) = Seq(
    "contravariant identity" -> forAll { (fa: F[A]) =>
      fa.contramap((a: A) => a) == fa
    },
    "contravariant composition" -> forAll { (fa: F[A], f: B => A, g: C => B) =>
      fa.contramap(f).contramap(g) == fa.contramap(g andThen f)
    }
  )

  def contravariant(implicit arbFInt: Arbitrary[F[Int]]): RuleSet = new RuleSet {
    def name = "contravariant"
    def bases = Nil
    def parents = Seq(exponential)
    def props = contravariantProperties[Int, String, Long]
  }
}

