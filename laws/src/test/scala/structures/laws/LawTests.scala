package structures
package laws

import org.typelevel.discipline.scalatest.Discipline
import org.scalatest.FunSuite

import structures.instances._

class LawTests extends FunSuite with Discipline {
  checkAll("List", ApplicativeLaws[List].applicative)
  checkAll("Option", ApplicativeLaws[Option].applicative)
}
