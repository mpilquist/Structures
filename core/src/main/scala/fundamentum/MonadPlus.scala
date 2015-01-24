package fundamentum

import simulacrum.typeclass

/**
 * Describes type constructors that have both a monad and a universally quantified monoid.
 *
 * Equivalently, this describes type constructors that have `MonadFilter` instances and additionally
 * define a universally quantified associative operation via the `plus` method.
 */
@typeclass trait MonadPlus[F[_]] extends MonadFilter[F] with PlusEmpty[F] {

  def unite[G[_], A](fga: F[G[A]])(implicit G: Foldable[G]): F[A] =
    flatMap(fga)(ga => G.foldLeft(ga, empty[A])((acc, a) => plus(acc, pure(a))))
}

