package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object MonadLaws {
  def apply[F[_]: Monad]: MonadLaws[F] = new MonadLaws[F] {
    def typeClass = Monad[F]
  }
}

trait MonadLaws[F[_]] extends FlatMapLaws[F] with ApplicativeLaws[F] {

  import Monad.ops._, Equal.ops._

  implicit def typeClass: Monad[F]

  def monadProperties[A, B, C](implicit
    arbA: Arbitrary[A],
    arbAtoFB: Arbitrary[A => F[B]],
    arbFA: Arbitrary[F[A]],
    arbBtoFC: Arbitrary[B => F[C]],
    eqFA: Equal[F[A]],
    eqFB: Equal[F[B]]
  ) = {
    val F = Monad[F]
    import F._
    Seq(
      "monad left identity" -> forAll { (a: A, f: A => F[B]) =>
        pure(a).flatMap(f) === f(a)
      },
      "monad right identity" -> forAll { (fa: F[A]) =>
        fa.flatMap { a => pure(a) } === fa
      }
    )
  }

  def monad(implicit
    arbFInt: Arbitrary[F[Int]],
    arbIntToFString: Arbitrary[Int => F[String]],
    arbStringToFLong: Arbitrary[String => F[Long]],
    arbFIntToString: Arbitrary[F[Int => String]],
    arbFStringToLong: Arbitrary[F[String => Long]],
    eqFInt: Equal[F[Int]],
    eqFLong: Equal[F[Long]],
    eqFStirng: Equal[F[String]]
  ): RuleSet = new RuleSet {
    def name = "monad"
    def bases = Nil
    def parents = Seq(flatMap, applicative)
    def props = monadProperties[Int, String, Long]
  }
}

