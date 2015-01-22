package fundamentum
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object ApplyLaws {
  def apply[F[_], A, B, C](implicit
    typeClass: Apply[F],
    arbFA: Arbitrary[F[A]],
    arbAtoB: Arbitrary[A => B],
    arbBtoA: Arbitrary[B => A],
    arbBtoC: Arbitrary[B => C],
    arbCtoB: Arbitrary[C => B],
    arbFAtoB: Arbitrary[F[A => B]],
    arbFBtoC: Arbitrary[F[B => C]]
  ): ApplyLaws[F, A, B, C] = {
    val typeClass0 = typeClass
    val arbFA0 = arbFA
    val arbAtoB0 = arbAtoB
    val arbBtoA0 = arbBtoA
    val arbBtoC0 = arbBtoC
    val arbCtoB0 = arbCtoB
    val arbFAtoB0 = arbFAtoB
    val arbFBtoC0 = arbFBtoC
    new ApplyLaws[F, A, B, C] {
      def typeClass = typeClass0
      def arbFA = arbFA0
      def arbAtoB = arbAtoB0
      def arbBtoA = arbBtoA0
      def arbBtoC = arbBtoC0
      def arbCtoB = arbCtoB0
      def arbFAtoB = arbFAtoB0
      def arbFBtoC = arbFBtoC0
    }
  }
}

trait ApplyLaws[F[_], A, B, C] extends FunctorLaws[F, A, B, C] {

  implicit def typeClass: Apply[F]

  implicit def arbFAtoB: Arbitrary[F[A => B]]
  implicit def arbFBtoC: Arbitrary[F[B => C]]

  def apply = new DefaultRuleSet(
    name = "apply",
    parent = Some(functor),
    props =
      "apply associative composition" -> forAll { (fa: F[A], fab: F[A => B], fbc: F[B => C]) =>
        typeClass.apply(typeClass.apply(fa)(fab))(fbc) ==
          typeClass.apply(fa)(typeClass.apply(fab)(typeClass.map(fbc)((bc: B => C) => (ab: A => B) => ab andThen bc)))
      }
  )
}

