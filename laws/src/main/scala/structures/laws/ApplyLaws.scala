package structures
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

  import Apply.Adapter

  implicit def typeClass: Apply[F]

  def applyProperties[A, B, C](implicit
    arbFA: Arbitrary[F[A]],
    arbFAtoB: Arbitrary[F[A => B]],
    arbFBtoC: Arbitrary[F[B => C]]
  ) = Seq(
    "apply associative composition" -> forAll { (fa: F[A], fab: F[A => B], fbc: F[B => C]) =>
      fa.apply(fab).apply(fbc) == fa.apply(fab.apply(fbc.map((bc: B => C) => (ab: A => B) => ab andThen bc)))
    }
  )

  def apply(implicit arbFInt: Arbitrary[F[Int]], arbFIntToString: Arbitrary[F[Int => String]], arbFStringToLong: Arbitrary[F[String => Long]]): RuleSet = new RuleSet {
    def name = "apply"
    def bases = Nil
    def parents = Seq(functor)
    def props = applyProperties[Int, String, Long]
  }
}

