package structures

import simulacrum.typeclass

@typeclass trait Plus[F[_]] { self =>
  def plus[A](x: F[A], y: => F[A]): F[A]
}
