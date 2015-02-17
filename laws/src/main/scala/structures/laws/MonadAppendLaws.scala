package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

object MonadCombineLaws {
  def apply[F[_]: MonadCombine]: MonadCombineLaws[F] = new MonadCombineLaws[F] {
    def typeClass = MonadCombine[F]
  }
}

trait MonadCombineLaws[F[_]] extends MonadFilterLaws[F] with MonoidKLaws[F] {

  import MonadCombine.ops._

  implicit def typeClass: MonadCombine[F]

  def monadAppend(implicit
    arbFInt: Arbitrary[F[Int]],
    arbIntToFString: Arbitrary[Int => F[String]],
    arbStringToFLong: Arbitrary[String => F[Long]],
    arbFIntToString: Arbitrary[F[Int => String]],
    arbFStringToLong: Arbitrary[F[String => Long]],
    eqFInt: Equal[F[Int]],
    eqFLong: Equal[F[Long]],
    eqFStirng: Equal[F[String]]
  ): RuleSet = new RuleSet {
    def name = "monad combine"
    def bases = Nil
    def parents = Seq(monadFilter, umonoid)
    def props = Nil
  }
}

