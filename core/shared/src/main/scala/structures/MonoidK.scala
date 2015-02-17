package structures

import simulacrum.typeclass

/**
 * Type class that describes type constructors which can provide a `Monoid[F[A]]` for any type `A`.
 */
@typeclass trait MonoidK[F[_]] extends Any with SemigroupK[F] { self =>

  def empty[A]: F[A]

  def toMonoid[A]: Monoid[F[A]] = new Monoid[F[A]] {
    def empty = self.empty[A]
    def combine(x: F[A], y: => F[A]) = combine(x, y)
  }
}
