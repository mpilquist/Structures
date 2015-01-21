package fundamentum
package laws

import org.typelevel.discipline.scalatest.Discipline
import org.scalatest.FunSuite

import fundamentum.instances._

class LawTests extends FunSuite with Discipline {
  checkAll("List", FunctorLaws[List, Int].functor[String, Int])
  checkAll("Option", FunctorLaws[Option, Int].functor[String, Int])
}
