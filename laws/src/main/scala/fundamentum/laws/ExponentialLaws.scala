package fundamentum
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

  implicit def typeClass: Exponential[F]

  def exponential[A, B, C](implicit
    arbFA: Arbitrary[F[A]],
    arbAtoB: Arbitrary[A => B],
    arbBtoA: Arbitrary[B => A],
    arbBtoC: Arbitrary[B => C],
    arbCtoB: Arbitrary[C => B]
  ) = new SimpleRuleSet(
    name = "exponential",
    props =
      "exponential identity" -> forAll { (fa: F[A]) => Exponential[F].xmap[A, A](fa)(x => x, x => x) == fa },
      "exponential composition" -> forAll { (fa: F[A], f1: A => B, f2: B => A, g1: B => C, g2: C => B) =>
        Exponential[F].xmap(Exponential[F].xmap(fa)(f1, f2))(g1, g2) == Exponential[F].xmap(fa)(f1 andThen g1, g2 andThen f2)
      }
  )
}

