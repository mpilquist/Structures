package structures
package laws

trait FunctorLaws[F[_]] extends ExponentialLaws[F] {

  implicit val typeClass: Functor[F]

  import Functor.ops._

  def functorIdentity[A](fa: F[A]): IsEqual[F[A]] =
    fa.map[A](identity) =?= fa

  def functorComposition[A, B, C](fa: F[A], f: A => B, g: B => C): IsEqual[F[C]] =
    fa.map(f).map(g) =?= fa.map(f andThen g)
}

object FunctorLaws {
  def apply[F[_]: Functor]: FunctorLaws[F] = new FunctorLaws[F] {
    val typeClass = Functor[F]
  }
}
