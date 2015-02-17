package structures
package laws

trait ContravariantLaws[F[_]] extends ExponentialLaws[F] {

  implicit val typeClass: Contravariant[F]

  import Contravariant.ops._

  def contravariantIdentity[A](fa: F[A]): IsEqual[F[A]] =
    fa.contramap[A](identity) =?= fa

  def contravariantComposition[A, B, C](fa: F[A], f: B => A, g: C => B): IsEqual[F[C]] =
    fa.contramap(f).contramap(g) =?= fa.contramap(g andThen f)
}

object ContravariantLaws {
  def apply[F[_]: Contravariant]: ContravariantLaws[F] = new ContravariantLaws[F] {
    val typeClass = Contravariant[F]
  }
}
