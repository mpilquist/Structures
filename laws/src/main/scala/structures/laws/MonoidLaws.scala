package structures
package laws

trait MonoidLaws[A] extends SemigroupLaws[A] {

  implicit val typeClass: Monoid[A]

  import Monoid.ops._

  def combineRightIdentity(x: A): IsEqual[A] =
    (x |+| typeClass.empty) =?= x

  def combineLeftIdentity(x: A): IsEqual[A] =
    (typeClass.empty |+| x) =?= x
}

object MonoidLaws {
  def apply[A: Monoid]: MonoidLaws[A] = new MonoidLaws[A] {
    val typeClass = Monoid[A]
  }
}
