package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait FlatMapDiscipline[F[_]] extends ApplyDiscipline[F] {

  def laws: FlatMapLaws[F]

  def flatMap[A, B, C](implicit
    arbFA: Arbitrary[F[A]],
    arbFB: Arbitrary[F[B]],
    arbFC: Arbitrary[F[C]],
    arbA: Arbitrary[A],
    arbB: Arbitrary[B],
    arbC: Arbitrary[C],
    arbFAtoB: Arbitrary[F[A => B]],
    arbFBtoC: Arbitrary[F[B => C]],
    eqFA: Equal[F[A]],
    eqFB: Equal[F[B]],
    eqFC: Equal[F[C]]
  ): RuleSet = new DefaultRuleSet(
    name = "flatMap",
    parent = Some(apply[A, B, C]),
    props =
      "flatMap associativity" -> forAll { (fa: F[A], f: A => F[B], g: B => F[C]) =>
        laws.flatMapAssociativity(fa, f, g).isEqual
      }
  )
}

object FlatMapDiscipline {
  def apply[F[_]: FlatMap]: FlatMapDiscipline[F] = new FlatMapDiscipline[F] {
    def laws = FlatMapLaws[F]
  }
}
