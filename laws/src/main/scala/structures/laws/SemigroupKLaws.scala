package structures
package laws

trait SemigroupKLaws[F[_]] {

  implicit val typeClass: SemigroupK[F]

  import SemigroupK.ops._

  def combineAssociativity[A](x: F[A], y: F[A], z: F[A]): IsEqual[F[A]] =
    ((x <+> y) <+> z) =?= (x <+> (y <+> z))
}

object SemigroupKLaws {
  def apply[F[_]: SemigroupK]: SemigroupKLaws[F] = new SemigroupKLaws[F] {
    val typeClass = SemigroupK[F]
  }
}
