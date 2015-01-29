package structures

import simulacrum.typeclass

/**
 * Type class that describes type constructors which can provide a `Monoid[F[A]]` for any type `A`.
 */
@typeclass trait UMonoid[F[_]] extends USemigroup[F] { self =>

  def id[A]: F[A]

  def toMonoid[A]: Monoid[F[A]] = new Monoid[F[A]] {
    def id = self.id[A]
    def append(x: F[A], y: => F[A]) = append(x, y)
  }
}
