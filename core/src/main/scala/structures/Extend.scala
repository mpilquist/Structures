package structures

import simulacrum.typeclass

@typeclass trait Extend[F[_]] extends Functor[F] {
  def extend[A, B](fa: F[A])(f: F[A] => B): F[B]
  def duplicated[A](fa: F[A]): F[F[A]] =
    extend(fa)(identity)
}
