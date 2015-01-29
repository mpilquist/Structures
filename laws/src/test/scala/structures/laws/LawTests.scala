package structures
package laws

import org.typelevel.discipline.scalatest.Discipline
import org.scalatest.FunSuite

import structures.std._

class LawTests extends FunSuite with Discipline {
  checkAll("List", ApplicativeLaws[List].applicative)
  checkAll("Option", ApplicativeLaws[Option].applicative)
  checkAll("Map", ApplyLaws[Map[Int, ?]].apply)
}
