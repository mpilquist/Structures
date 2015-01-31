package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object FlatMapLaws {
  def apply[F[_]: FlatMap]: FlatMapLaws[F] = new FlatMapLaws[F] {
    def typeClass = FlatMap[F]
  }
}

trait FlatMapLaws[F[_]] extends ApplyLaws[F] {

  import FlatMap.ops._, Equal.ops._

  implicit def typeClass: FlatMap[F]

  def flatMapProperties[A, B, C](implicit
    arbFA: Arbitrary[F[A]],
    arbAtoFB: Arbitrary[A => F[B]],
    arbBtoFC: Arbitrary[B => F[C]],
    eqFC: Equal[F[C]]
  ) = {
    val F = FlatMap[F]
    import F._
    Seq(
      "flatMap associativity" -> forAll { (fa: F[A], f: A => F[B], g: B => F[C]) =>
        fa.flatMap(f).flatMap(g) === fa.flatMap { a => f(a).flatMap(g) }
      }
    )
  }

  def flatMap(implicit
    arbFInt: Arbitrary[F[Int]],
    arbIntToFString: Arbitrary[Int => F[String]],
    arbStringToFLong: Arbitrary[String => F[Long]],
    arbFIntToString: Arbitrary[F[Int => String]],
    arbFStringToLong: Arbitrary[F[String => Long]],
    eqFInt: Equal[F[Int]],
    eqFLong: Equal[F[Long]],
    eqFStirng: Equal[F[String]]
  ): RuleSet = new RuleSet {
    def name = "flatMap"
    def bases = Nil
    def parents = Seq(apply)
    def props = flatMapProperties[Int, String, Long]
  }
}

