package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object MonadAppendLaws {
  def apply[F[_]: MonadAppend]: MonadAppendLaws[F] = new MonadAppendLaws[F] {
    def typeClass = MonadAppend[F]
  }
}

trait MonadAppendLaws[F[_]] extends MonadFilterLaws[F] with UMonoidLaws[F] {

  import MonadAppend.Adapter

  implicit def typeClass: MonadAppend[F]

  def monadAppend(implicit
    arbFInt: Arbitrary[F[Int]],
    arbIntToFString: Arbitrary[Int => F[String]],
    arbStringToFLong: Arbitrary[String => F[Long]],
    arbFIntToString: Arbitrary[F[Int => String]],
    arbFStringToLong: Arbitrary[F[String => Long]]
  ): RuleSet = new RuleSet {
    def name = "monad append"
    def bases = Nil
    def parents = Seq(monadFilter, umonoid)
    def props = Nil
  }
}



