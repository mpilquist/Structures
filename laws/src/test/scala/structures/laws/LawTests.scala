package structures
package laws

import org.typelevel.discipline.scalatest.Discipline
import org.scalatest.FunSuite
import org.scalacheck.Arbitrary

import structures.std._

class LawTests extends FunSuite with Discipline {
  checkAll("Byte", MonoidLaws[Byte].monoid)
  checkAll("Short", MonoidLaws[Short].monoid)
  checkAll("Int", MonoidLaws[Int].monoid)
  checkAll("Long", MonoidLaws[Long].monoid)
  checkAll("String", MonoidLaws[String].monoid)

  checkAll("List", MonadAppendLaws[List].monadAppend)
  checkAll("Option", MonadAppendLaws[Option].monadAppend)
  checkAll("Map", FlatMapLaws[Map[Int, ?]].flatMap)


  checkAll("Composite Applicative", ApplicativeLaws[Lambda[X => List[Option[X]]]](Applicative[List] compose Applicative[Option]).applicative)

  implicit def function1Equal[A, B](implicit arbA: Arbitrary[A], eqB: Equal[B]): Equal[A => B] = Equal.instance { (x, y) =>
    val samples = Stream.continually(arbA.arbitrary.sample).flatMap(x => x).take(100)
    samples.forall { a => x(a) === y(a) }
  }

  checkAll("Function1", MonadLaws[Int => ?].monad)
  checkAll("Function1", ContravariantLaws[? => Int].contravariant)

  implicit def arbOrdering[A]: Arbitrary[Ordering[A]] = Arbitrary(Arbitrary.arbitrary[(A, A) => Boolean].map { f => Ordering.fromLessThan(f) })
  implicit def orderingEqual[A](implicit arbA: Arbitrary[A]): Equal[Ordering[A]] = Equal.instance { (x, y) =>
    val samples = Stream.continually(Arbitrary.arbitrary[(A, A)].sample).flatMap(x => x).take(100)
    samples.forall { case (l, r) => x.compare(l, r) == y.compare(l, r) }
  }
  checkAll("Ordering", ContravariantLaws[Ordering].contravariant)
}
