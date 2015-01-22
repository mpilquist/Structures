package fundamentum

import simulacrum.typeclass

/**
 * Describes type constructors that support the `xmap` method such that:
 *  - `xmap(fa)(identity, identity) == fa`
 *  - `xmap(xmap(fa)(f1, f2))(g1, g2) == xmap(fa)(f1 andThen g1, g2 andThen f2)`
 *
 * The name is short for "exponential functor", which is also known as "invariant functor".
 */
@typeclass trait Exponential[F[_]] {

  /**
   * Converts the supplied `F[A]` to an `F[B]` using a pair of functions -- `A => B` and `B => A`.
   */
  def xmap[A, B](fa: F[A])(f: A => B, g: B => A): F[B]
}
