package fundamentum

import simulacrum.typeclass

@typeclass trait Applicative[F[_]] extends Apply[F] with Functor[F] {

  def pure[A](a: A): F[A]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(fa)(pure(f))
}
