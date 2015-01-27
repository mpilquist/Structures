package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object ExponentialLaws {
  def apply[F[_]: Exponential]: ExponentialLaws[F] = new ExponentialLaws[F] {
    def typeClass = Exponential[F]
  }
}

trait ExponentialLaws[F[_]] extends Laws {

  import Exponential.Adapter

  implicit def typeClass: Exponential[F]

  def exponentialProperties[A, B, C](implicit
    arbFA: Arbitrary[F[A]],
    arbAtoB: Arbitrary[A => B],
    arbBtoA: Arbitrary[B => A],
    arbBtoC: Arbitrary[B => C],
    arbCtoB: Arbitrary[C => B]
  ) = Seq(
    "exponential identity" -> forAll { (fa: F[A]) =>
      fa.xmap[A](identity, identity) == fa
    },
    "exponential composition" -> forAll { (fa: F[A], f1: A => B, f2: B => A, g1: B => C, g2: C => B) =>
      fa.xmap(f1, f2).xmap(g1, g2) == fa.xmap(f1 andThen g1, g2 andThen f2)
    }
  )

  def exponential(implicit arbFInt: Arbitrary[F[Int]]): RuleSet = new RuleSet {
    def name = "exponential"
    def bases = Nil
    def parents = Nil
    def props = exponentialProperties[Int, String, Long]
  }
}

