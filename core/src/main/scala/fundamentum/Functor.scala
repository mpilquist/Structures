package fundamentum

import simulacrum.typeclass


@typeclass trait Functor[F[_]] extends Exponential[F] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  override def xmap[A, B](fa: F[A])(f: A => B, g: B => A): F[B] =
    map(fa)(f)
}
