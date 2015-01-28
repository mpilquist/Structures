package structures

import simulacrum.typeclass

@typeclass trait Plus[F[_]] { self =>
  def plus[A](x: F[A], y: => F[A]): F[A]

  def toSemigroup[A]: Semigroup[F[A]] = new Semigroup[F[A]] {
    def append(x: F[A], y: => F[A]) = plus(x, y)
  }
}
