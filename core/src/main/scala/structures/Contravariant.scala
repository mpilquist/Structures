package structures

import simulacrum.typeclass

@typeclass trait Contravariant[F[_]] extends Any with Exponential[F] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
  override def xmap[A, B](fa: F[A])(f: A => B, g: B => A): F[B] =
    contramap(fa)(g)
}
