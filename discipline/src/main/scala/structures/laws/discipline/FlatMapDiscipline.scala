package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait FlatMapDiscipline[F[_]] extends ApplyDiscipline[F] {

  def laws: FlatMapLaws[F]

  def flatMap[A: Arbitrary: Equal, B: Arbitrary: Equal, C: Arbitrary: Equal]: RuleSet = new DefaultRuleSet(
    name = "flatMap",
    parent = Some(apply[A, B, C]),
    props =
      "flatMap associativity" -> forAll { (fa: F[A], f: A => F[B], g: B => F[C]) =>
        laws.flatMapAssociativity(fa, f, g).isEqual
      }
  )
}

object FlatMapDiscipline {
  def apply[F[_]](implicit TC: FlatMap[F], A: ArbitraryK[F], E: EqualK[F]): FlatMapDiscipline[F] = new FlatMapDiscipline[F] {
    def laws = FlatMapLaws[F]
    def arbitraryKF = A
    def equalKF = E
  }
}
