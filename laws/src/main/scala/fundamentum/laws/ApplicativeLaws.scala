package fundamentum
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object ApplicativeLaws {
  def apply[F[_]: Applicative]: ApplicativeLaws[F] = new ApplicativeLaws[F] {
    def typeClass = Applicative[F]
  }
}

trait ApplicativeLaws[F[_]] extends ApplyLaws[F] {

  import Applicative.Adapter

  implicit def typeClass: Applicative[F]

  def applicativeProperties[A, B, C](implicit
    arbA: Arbitrary[A],
    arbAB: Arbitrary[A => B],
    arbFA: Arbitrary[F[A]],
    arbFAtoB: Arbitrary[F[A => B]],
    arbFBtoC: Arbitrary[F[B => C]]
  ) = {
    val F = Applicative[F]
    import F._
    Seq(
      "applicative identity" -> forAll { (fa: F[A]) =>
        fa.apply(pure((a: A) => a)) == fa
      },
      "applicative composition" -> forAll { (fa: F[A], fab: F[A => B], fbc: F[B => C]) =>
        fa.apply(fab).apply(fbc) == fa.apply(fab.apply(fbc.apply(pure((bc: B => C) => (ab: A => B) => ab andThen bc))))
      },
      "applicative homomorphism" -> forAll { (a: A, f: A => B) =>
        pure(a).apply(pure(f)) == pure(f(a))
      },
      "applicative interchange" -> forAll { (a: A, fab: F[A => B]) =>
        pure(a).apply(fab) == fab.apply(pure((f: A => B) => f(a)))
      }
    )
  }

  def applicative(implicit arbFInt: Arbitrary[F[Int]], arbFIntToString: Arbitrary[F[Int => String]], arbFStringToLong: Arbitrary[F[String => Long]]): RuleSet = new RuleSet {
    def name = "applicative"
    def bases = Nil
    def parents = Seq(apply)
    def props = applicativeProperties[Int, String, Long]
  }
}


