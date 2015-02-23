package structures
package laws

trait MonoidKLaws[F[_]] extends SemigroupKLaws[F] {

  implicit val typeClass: MonoidK[F]

  import MonoidK.ops._

  def combineRightIdentity[A](x: F[A]): IsEqual[F[A]] =
    (x <+> typeClass.empty) =?= x

  def combineLeftIdentity[A](x: F[A]): IsEqual[F[A]] =
    (typeClass.empty <+> x) =?= x
}

object MonoidKLaws {
  def apply[F[_]: MonoidK]: MonoidKLaws[F] = new MonoidKLaws[F] {
    val typeClass = MonoidK[F]
  }
}
