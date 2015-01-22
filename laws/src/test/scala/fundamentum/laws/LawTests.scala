package fundamentum
package laws

import org.typelevel.discipline.scalatest.Discipline
import org.scalatest.FunSuite

import fundamentum.instances._

class LawTests extends FunSuite with Discipline {
  checkAll("List", ApplyLaws[List, Int, String, Int].apply)
  checkAll("Option", ApplyLaws[Option, Int, String, Int].apply)
}
