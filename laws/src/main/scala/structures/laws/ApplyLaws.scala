package structures
package laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

object ApplyLaws {
  def apply[F[_]: Apply]: ApplyLaws[F] = new ApplyLaws[F] {
    def typeClass = Apply[F]
  }
}

trait ApplyLaws[F[_]] extends FunctorLaws[F] {

  import Apply.ops._, Equal.ops._

  implicit def typeClass: Apply[F]

  def applyProperties[A, B, C](implicit
    arbFA: Arbitrary[F[A]],
    arbFAtoB: Arbitrary[F[A => B]],
    arbFBtoC: Arbitrary[F[B => C]],
    eqFC: Equal[F[C]]
  ) = Seq(
    "apply associative composition" -> forAll { (fa: F[A], fab: F[A => B], fbc: F[B => C]) =>
      fa.apply(fab).apply(fbc) === fa.apply(fab.apply(fbc.map((bc: B => C) => (ab: A => B) => ab andThen bc)))
    }
  )

  def apply(implicit
    arbFInt: Arbitrary[F[Int]],
    arbFIntToString: Arbitrary[F[Int => String]],
    arbFStringToLong: Arbitrary[F[String => Long]],
    eqFInt: Equal[F[Int]],
    eqFLong: Equal[F[Long]]
  ): RuleSet = new RuleSet {
    def name = "apply"
    def bases = Nil
    def parents = Seq(functor)
    def props = applyProperties[Int, String, Long]
  }
}

