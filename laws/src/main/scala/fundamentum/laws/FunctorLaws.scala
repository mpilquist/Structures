package fundamentum
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object FunctorLaws {
  def apply[F[_], A, B, C](implicit
    typeClass: Functor[F],
    arbFA: Arbitrary[F[A]],
    arbAtoB: Arbitrary[A => B],
    arbBtoA: Arbitrary[B => A],
    arbBtoC: Arbitrary[B => C],
    arbCtoB: Arbitrary[C => B]
  ): FunctorLaws[F, A, B, C] = {
    val typeClass0 = typeClass
    val arbFA0 = arbFA
    val arbAtoB0 = arbAtoB
    val arbBtoA0 = arbBtoA
    val arbBtoC0 = arbBtoC
    val arbCtoB0 = arbCtoB
    new FunctorLaws[F, A, B, C] {
      def typeClass = typeClass0
      def arbFA = arbFA0
      def arbAtoB = arbAtoB0
      def arbBtoA = arbBtoA0
      def arbBtoC = arbBtoC0
      def arbCtoB = arbCtoB0
    }
  }
}

trait FunctorLaws[F[_], A, B, C] extends ExponentialLaws[F, A, B, C] {

  implicit def typeClass: Functor[F]

  def functor = new DefaultRuleSet(
    name = "functor",
    parent = Some(exponential),
    props =
      "covariant identity" -> forAll { (fa: F[A]) => Functor[F].map(fa)(x => x) == fa },
      "covariant composition" -> forAll { (fa: F[A], f: A => B, g: B => C) =>
        Functor[F].map(Functor[F].map(fa)(f))(g) == Functor[F].map(fa)(f andThen g)
      }
  )
}
