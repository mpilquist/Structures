package structures
package laws
package discipline

import org.typelevel.discipline.scalatest.Discipline
import org.scalatest.FunSuite
import org.scalacheck.{ Arbitrary, Gen }
import Arbitrary.arbitrary

import structures.std._

class LawTests extends FunSuite with Discipline {

  implicit val arbKList: ArbitraryK[List] = new ArbitraryK[List] {
    def toArbitrary[A: Arbitrary] = Arbitrary(Gen.listOf(arbitrary[A]))
  }

  implicit val arbKOption: ArbitraryK[Option] = new ArbitraryK[Option] {
    def toArbitrary[A: Arbitrary] = Arbitrary(Gen.oneOf(Gen.const(None: Option[A]), arbitrary[A].map(Some.apply)))
  }

/*
  implicit def arbKMap[K: Arbitrary]: ArbitraryK[Map[K, ?]] = new ArbitraryK[Map[K, ?]] {
    def toArbitrary[V: Arbitrary] = ???
  }

  implicit def arbKFunction1L[A: Arbitrary]: ArbitraryK[A => ?] = new ArbitraryK[A => ?] {
    def toArbitrary[B: Arbitrary] = ???
  }
  implicit def arbKFunction1R[B: Arbitrary]: ArbitraryK[? => B] = new ArbitraryK[? => B] {
    def toArbitrary[A: Arbitrary] = ???
  }
  */

  checkAll("Byte", MonoidDiscipline[Byte].monoid)
  checkAll("Short", MonoidDiscipline[Short].monoid)
  checkAll("Int", MonoidDiscipline[Int].monoid)
  checkAll("Long", MonoidDiscipline[Long].monoid)
  checkAll("String", MonoidDiscipline[String].monoid)

  checkAll("List", MonadCombineDiscipline[List].monadCombine[Int, String, Long])
  checkAll("Option", MonadCombineDiscipline[Option].monadCombine[Int, String, Long])
  //checkAll("Map", FlatMapDiscipline[Map[Int, ?]].flatMap[Int, String, Long])

//  checkAll("Composite Applicative", ApplicativeDiscipline[Lambda[X => List[Option[X]]]](Applicative[List] compose Applicative[Option]).applicative[Int, String, Long])

  implicit def equalKFunction1L[A: Arbitrary]: EqualK[A => ?] = new EqualK[A => ?] {
    def equal[B](x: A => B, y: A => B)(implicit B: Equal[B]) = {
      val samples = Stream.continually(arbitrary[A].sample).flatMap(x => x).take(100)
      samples.forall { a => x(a) === y(a) }
    }
  }

  //checkAll("Function1", MonadDiscipline[Int => ?].monad[Int, String, Long])
  //checkAll("Function1", ContravariantDiscipline[? => Int].contravariant[Int, String, Long])

  implicit val arbKOrdering: ArbitraryK[Ordering] = new ArbitraryK[Ordering] {
    def toArbitrary[A: Arbitrary] = Arbitrary(arbitrary[(A, A) => Boolean].map { f => Ordering.fromLessThan(f) })
  }
  /*
  implicit val equalKOrdering: EqualK[Ordering] = new EqualK[Ordering] {
    def equal[A](x: Ordering[A], y: Ordering[A]) = {
      val samples = Stream.continually(arbitrary[(A, A)].sample).flatMap(x => x).take(100)
      samples.forall { case (l, r) => x.compare(l, r) == y.compare(l, r) }
    }
  }
  checkAll("Ordering", ContravariantDiscipline[Ordering].contravariant[Int, String, Long])
  */
}
