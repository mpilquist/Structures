package structures

import simulacrum.typeclass

@typeclass trait PlusEmpty[F[_]] extends Plus[F] { self =>
  def empty[A]: F[A]

  def toMonoid[A]: Monoid[F[A]] = new Monoid[F[A]] {
    def id = empty[A]
    def append(x: F[A], y: => F[A]) = plus(x, y)
  }
}
