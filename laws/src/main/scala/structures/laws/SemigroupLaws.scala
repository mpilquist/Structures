package structures
package laws

trait SemigroupLaws[A] {

  implicit val typeClass: Semigroup[A]

  import Semigroup.ops._

  def combineAssociativity(x: A, y: A, z: A): IsEqual[A] =
    ((x |+| y) |+| z) =?= (x |+| (y |+| z))
}

object SemigroupLaws {
  def apply[A: Semigroup]: SemigroupLaws[A] = new SemigroupLaws[A] {
    val typeClass = Semigroup[A]
  }
}
