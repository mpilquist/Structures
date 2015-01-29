package structures

import simulacrum.typeclass

/**
 * Type class that describes type constructors that support a `map` method which adheres
 * to the laws described in [[structures.laws.FunctorLaws]].
 *
 * The name is short for "covariant functor".
 *
 * Note that every functor is an exponential functor, where `xmap` is implemented in
 * terms of `map` by ignoring the `B => A` function.
 */
@typeclass trait Functor[F[_]] extends Any with Exponential[F] {

  /**
   * Converts the supplied `F[A]` in to an `F[B]` using the supplied `A => B`.
   */
  def map[A, B](fa: F[A])(f: A => B): F[B]

  override def xmap[A, B](fa: F[A])(f: A => B, g: B => A): F[B] =
    map(fa)(f)

  /** Lifts the supplied function in to the `F` type constructor. */
  def lift[A, B](f: A => B): F[A] => F[B] =
    fa => map(fa)(f)

  /** Replaces the `A` value in `F[A]` with the supplied value. */
  def as[A, B](fa: F[A], b: B): F[B] =
    map(fa)(_ => b)

  /**
   * Maps the supplied function over `F[A]`, returning the original value
   * and the result of the function application.
   */
  def zipWith[A, B](fa: F[A])(f: A => B): F[(A, B)] =
    map(fa)(a => (a, f(a)))
}
