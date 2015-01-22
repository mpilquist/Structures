package fundamentum
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object ExponentialLaws {
  def apply[F[_], A, B, C](implicit
    typeClass: Exponential[F],
    arbFA: Arbitrary[F[A]],
    arbAtoB: Arbitrary[A => B],
    arbBtoA: Arbitrary[B => A],
    arbBtoC: Arbitrary[B => C],
    arbCtoB: Arbitrary[C => B]
  ): ExponentialLaws[F, A, B, C] = {
    val typeClass0 = typeClass
    val arbFA0 = arbFA
    val arbAtoB0 = arbAtoB
    val arbBtoA0 = arbBtoA
    val arbBtoC0 = arbBtoC
    val arbCtoB0 = arbCtoB
    new ExponentialLaws[F, A, B, C] {
      def typeClass = typeClass0
      def arbFA = arbFA0
      def arbAtoB = arbAtoB0
      def arbBtoA = arbBtoA0
      def arbBtoC = arbBtoC0
      def arbCtoB = arbCtoB0
    }
  }
}

trait ExponentialLaws[F[_], A, B, C] extends Laws {

  implicit def typeClass: Exponential[F]

  implicit def arbFA: Arbitrary[F[A]]
  implicit def arbAtoB: Arbitrary[A => B]
  implicit def arbBtoA: Arbitrary[B => A]
  implicit def arbBtoC: Arbitrary[B => C]
  implicit def arbCtoB: Arbitrary[C => B]

  def exponential = new SimpleRuleSet(
    name = "exponential",
    props =
      "exponential identity" -> forAll { (fa: F[A]) => Exponential[F].xmap[A, A](fa)(x => x, x => x) == fa },
      "exponential composition" -> forAll { (fa: F[A], f1: A => B, f2: B => A, g1: B => C, g2: C => B) =>
        Exponential[F].xmap(Exponential[F].xmap(fa)(f1, f2))(g1, g2) == Exponential[F].xmap(fa)(f1 andThen g1, g2 andThen f2)
      }
  )
}

