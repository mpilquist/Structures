package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object MonadFilterLaws {
  def apply[F[_]: MonadFilter]: MonadFilterLaws[F] = new MonadFilterLaws[F] {
    def typeClass = MonadFilter[F]
  }
}

trait MonadFilterLaws[F[_]] extends MonadLaws[F] {

  import MonadFilter.ops._, Equal.ops._

  implicit def typeClass: MonadFilter[F]

  def monadFilterProperties[A, B](implicit
    arbAtoFB: Arbitrary[A => F[B]],
    arbFA: Arbitrary[F[A]],
    eqFA: Equal[F[A]],
    eqFB: Equal[F[B]]
  ) = {
    val F = MonadFilter[F]
    import F._
    Seq(
      "monad empty flatMap" -> forAll { (f: A => F[B]) =>
        empty[A].flatMap(f) === empty[B]
      },
      "monad flatMap empty" -> forAll { (fa: F[A]) =>
        fa.flatMap { a => empty[B] } === empty[B]
      }
    )
  }

  def monadFilter(implicit
    arbIntToFString: Arbitrary[Int => F[String]],
    arbFInt: Arbitrary[F[Int]],
    arbStringToFLong: Arbitrary[String => F[Long]],
    arbFIntToString: Arbitrary[F[Int => String]],
    arbFStringToLong: Arbitrary[F[String => Long]],
    eqFInt: Equal[F[Int]],
    eqFLong: Equal[F[Long]],
    eqFStirng: Equal[F[String]]
  ): RuleSet = new RuleSet {
    def name = "monad fitler"
    def bases = Nil
    def parents = Seq(monad)
    def props = monadFilterProperties[Int, String]
  }
}


