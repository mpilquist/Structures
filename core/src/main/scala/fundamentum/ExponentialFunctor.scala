package fundamentum

import simulacrum.typeclass

@typeclass trait ExponentialFunctor[F[_]] {
  def xmap[A, B](fa: F[A])(f: A => B, g: B => A): F[B]
}
