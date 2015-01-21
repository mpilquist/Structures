package fundamentum
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object FunctorLaws {
  def apply[F[_], A](implicit F: Functor[F], Arb: Arbitrary[F[A]]): FunctorLaws[F, A] = new FunctorLaws[F, A] {
    def functorF = F
    def arbFA = Arb
  }
}

trait FunctorLaws[F[_], A] extends Laws {

  implicit def functorF: Functor[F]
  implicit def arbFA: Arbitrary[F[A]]

  def functor[B, C](implicit arbAToB: Arbitrary[A => B], arbBToC: Arbitrary[B => C]) = new SimpleRuleSet(
    name = "functor",
    props =
      "identity" -> forAll { (fa: F[A]) => Functor[F].map(fa)(x => x) == fa },
      "composition" -> forAll { (fa: F[A], f: A => B, g: B => C) =>
        Functor[F].map(Functor[F].map(fa)(f))(g) == Functor[F].map(fa)(f andThen g)
      }
  )
}
