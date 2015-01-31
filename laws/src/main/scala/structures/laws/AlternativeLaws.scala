package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object AlternativeLaws {
  def apply[F[_]: Alternative]: AlternativeLaws[F] = new AlternativeLaws[F] {
    def typeClass = Alternative[F]
  }
}

trait AlternativeLaws[F[_]] extends ApplicativeLaws[F] with UMonoidLaws[F] {

  implicit def typeClass: Alternative[F]

  def alternative(implicit
    arbFInt: Arbitrary[F[Int]],
    arbFIntToString: Arbitrary[F[Int => String]],
    arbFStringToLong: Arbitrary[F[String => Long]],
    eqFInt: Equal[F[Int]],
    eqFLong: Equal[F[Long]],
    eqFStirng: Equal[F[String]]
  ): RuleSet = new RuleSet {
    def name = "alternative"
    def bases = Nil
    def parents = Seq(applicative, umonoid)
    def props = Nil
  }
}

