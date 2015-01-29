package structures

import simulacrum.typeclass

/**
 * Describes type constructors that have both a monad and a universally quantified monoid.
 *
 * Equivalently, this describes type constructors that have `MonadFilter` instances and additionally
 * define a universally quantified associative operation via the `plus` method.
 */
@typeclass trait MonadPlus[F[_]] extends MonadFilter[F] with Alternative[F] {

  def unite[G[_]: Foldable, A](fga: F[G[A]]): F[A] =
    flatMap(fga)(ga => Foldable[G].foldMap(ga)(a => pure(a))(toMonoid[A]))
}

