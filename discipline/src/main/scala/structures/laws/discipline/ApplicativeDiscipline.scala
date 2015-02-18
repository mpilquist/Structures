package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait ApplicativeDiscipline[F[_]] extends ApplyDiscipline[F] {

  def laws: ApplicativeLaws[F]

  def applicative[A: Arbitrary: Equal, B: Arbitrary: Equal, C: Arbitrary: Equal]: RuleSet = new DefaultRuleSet(
    name = "applicative",
    parent = Some(apply[A, B, C]),
    props =
      "applicative identity" -> forAll { (fa: F[A]) =>
        laws.applicativeIdentity(fa).isEqual
      },
      "applicative composition" -> forAll { (fa: F[A], fab: F[A => B], fbc: F[B => C]) =>
        laws.applicativeComposition(fa, fab, fbc).isEqual
      },
      "applicative homomorphism" -> forAll { (a: A, f: A => B) =>
        laws.applicativeHomomorphism(a, f).isEqual
      },
      "applicative interchange" -> forAll { (a: A, fab: F[A => B]) =>
        laws.applicativeInterchange(a, fab).isEqual
      },
      "applicative map consistency" -> forAll { (fa: F[A], f: A => B) =>
        laws.applicativeMapConsistency(fa, f).isEqual
      }
  )
}

object ApplicativeDiscipline {
  def apply[F[_]](implicit TC: Applicative[F], A: ArbitraryK[F], E: EqualK[F]): ApplicativeDiscipline[F] = new ApplicativeDiscipline[F] {
    def laws = ApplicativeLaws[F]
    def arbitraryKF = A
    def equalKF = E
  }
}
