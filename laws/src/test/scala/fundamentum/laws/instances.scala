package fundamentum
package laws

import org.typelevel.discipline.scalatest.Discipline
import org.scalatest.FunSuite

import fundamentum.instances._

class LawTests extends FunSuite with Discipline {
  checkAll("List", FunctorLaws[List, Int, String, Int].functor)
  checkAll("Option", FunctorLaws[Option, Int, String, Int].functor)
}
