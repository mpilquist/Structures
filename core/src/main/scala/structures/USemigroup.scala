package structures

import simulacrum.{ typeclass, op }

/**
 * Type class that describes type constructors which can provide a `Semigroup[F[A]]` for any type `A`.
 */
@typeclass trait USemigroup[F[_]] extends Any { self =>

  @op("|+|")
  def append[A](x: F[A], y: => F[A]): F[A]

  def toSemigroup[A]: Semigroup[F[A]] = new Semigroup[F[A]] {
    def append(x: F[A], y: => F[A]) = self.append(x, y)
  }
}
