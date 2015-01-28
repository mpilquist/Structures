package structures

import simulacrum.typeclass

/**
 * Type class that describes type constructors that support an `xmap` method which adheres
 * to the laws described in [[structures.laws.ExponentialLaws]].
 *
 * The name is short for "exponential functor", which is also known as "invariant functor".
 */
@typeclass trait Exponential[F[_]] {

  /**
   * Converts the supplied `F[A]` to an `F[B]` using a pair of functions -- `A => B` and `B => A`.
   */
  def xmap[A, B](fa: F[A])(f: A => B, g: B => A): F[B]
}
