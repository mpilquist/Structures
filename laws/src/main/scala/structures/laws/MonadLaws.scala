package structures
package laws

trait MonadLaws[F[_]] extends FlatMapLaws[F] with ApplicativeLaws[F] {

  implicit val typeClass: Monad[F]

  import Monad.ops._, typeClass.pure

  def monadLeftIdentity[A, B](a: A, f: A => F[B]): IsEqual[F[B]] =
    pure(a).flatMap(f) =?= f(a)

  def monadRightIdentity[A](fa: F[A]): IsEqual[F[A]] =
    fa.flatMap { a => pure(a) } =?= fa
}

object MonadLaws {
  def apply[F[_]: Monad]: MonadLaws[F] = new MonadLaws[F] {
    val typeClass = Monad[F]
  }
}
