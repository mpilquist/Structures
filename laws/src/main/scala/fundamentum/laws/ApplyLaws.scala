package fundamentum
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object ApplyLaws {
  def apply[F[_]: Apply]: ApplyLaws[F] = new ApplyLaws[F] {
    def typeClass = Apply[F]
  }
}

trait ApplyLaws[F[_]] extends FunctorLaws[F] {

  implicit def typeClass: Apply[F]

  def apply[A, B, C](implicit
    arbFA: Arbitrary[F[A]],
    arbAtoB: Arbitrary[A => B],
    arbBtoA: Arbitrary[B => A],
    arbBtoC: Arbitrary[B => C],
    arbCtoB: Arbitrary[C => B],
    arbFAtoB: Arbitrary[F[A => B]],
    arbFBtoC: Arbitrary[F[B => C]]
  ): RuleSet = new DefaultRuleSet(
    name = "apply",
    parent = Some(functor[A, B, C]),
    props =
      "apply associative composition" -> forAll { (fa: F[A], fab: F[A => B], fbc: F[B => C]) =>
        typeClass.apply(typeClass.apply(fa)(fab))(fbc) ==
          typeClass.apply(fa)(typeClass.apply(fab)(typeClass.map(fbc)((bc: B => C) => (ab: A => B) => ab andThen bc)))
      }
  )
}

