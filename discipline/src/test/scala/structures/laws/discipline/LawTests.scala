package structures
package laws
package discipline

import org.typelevel.discipline.scalatest.Discipline
import org.scalatest.FunSuite
import org.scalacheck.Arbitrary

import structures.std._

class LawTests extends FunSuite with Discipline {
  checkAll("Byte", MonoidDiscipline[Byte].monoid)
  checkAll("Short", MonoidDiscipline[Short].monoid)
  checkAll("Int", MonoidDiscipline[Int].monoid)
  checkAll("Long", MonoidDiscipline[Long].monoid)
  checkAll("String", MonoidDiscipline[String].monoid)
  checkAll("List", MonoidDiscipline[List[Int]].monoid)

  checkAll("List", MonadCombineDiscipline[List].monadCombine[Int, String, Long])
  checkAll("Option", MonadCombineDiscipline[Option].monadCombine[Int, String, Long])
  checkAll("Map", FlatMapDiscipline[Map[Int, ?]].flatMap[Int, String, Long])

  checkAll("Composite Applicative", ApplicativeDiscipline[Lambda[X => List[Option[X]]]](Applicative[List] compose Applicative[Option]).applicative[Int, String, Long])

  implicit def function1Equal[A, B](implicit arbA: Arbitrary[A], eqB: Equal[B]): Equal[A => B] = Equal.instance { (x, y) =>
    val samples = Stream.continually(arbA.arbitrary.sample).flatMap(x => x).take(100)
    samples.forall { a => x(a) === y(a) }
  }

  checkAll("Function1", MonadDiscipline[Int => ?].monad[Int, String, Long])
  checkAll("Function1", ContravariantDiscipline[? => Int].contravariant[Int, String, Long])

  implicit def arbOrdering[A]: Arbitrary[Ordering[A]] = Arbitrary(Arbitrary.arbitrary[(A, A) => Boolean].map { f => Ordering.fromLessThan(f) })
  implicit def orderingEqual[A](implicit arbA: Arbitrary[A]): Equal[Ordering[A]] = Equal.instance { (x, y) =>
    val samples = Stream.continually(Arbitrary.arbitrary[(A, A)].sample).flatMap(x => x).take(100)
    samples.forall { case (l, r) => x.compare(l, r) == y.compare(l, r) }
  }
  checkAll("Ordering", ContravariantDiscipline[Ordering].contravariant[Int, String, Long])
}
