package structures
package laws

import org.typelevel.discipline.scalatest.Discipline
import org.scalatest.FunSuite

import structures.std._

class LawTests extends FunSuite with Discipline {
  checkAll("Byte", MonoidLaws[Byte].monoid)
  checkAll("Short", MonoidLaws[Short].monoid)
  checkAll("Int", MonoidLaws[Int].monoid)
  checkAll("Long", MonoidLaws[Long].monoid)
  checkAll("String", MonoidLaws[String].monoid)

  checkAll("List", MonadAppendLaws[List].monadAppend)
  checkAll("Option", MonadAppendLaws[Option].monadAppend)
  checkAll("Map", ApplyLaws[Map[Int, ?]].apply)


  checkAll("Composite Applicative", ApplicativeLaws[Lambda[X => List[Option[X]]]](Applicative[List] compose Applicative[Option]).applicative)

  /*
  Function 1 tests fail b/c functions do not support ==
  checkAll("Function1", MonadLaws[Int => ?].monad)
  checkAll("Function1", ContravariantLaws[? => Int].contravariant)

  Ordering tests fail b/c ordering does not support ==
  checkAll("Ordering", ContravariantLaws[Ordering].contravariant)
  */
}
