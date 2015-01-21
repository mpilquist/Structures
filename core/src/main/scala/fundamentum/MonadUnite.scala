package fundamentum

import simulacrum.typeclass

/**
 * Describes the class of type constructors that have both a monad and a universally quantifier monoid.
 *
 * Equivalently, this describes type constructors that have `MonadFilter` instances and additionally
 * define a universally quantified associative operation via the `append` method.
 *
 * Note that the `empty` method, from `MonadFilter`, is defined in terms of `id` from `UniversalMonoid`.
 */
@typeclass trait MonadUnite[F[_]] extends MonadFilter[F] with UniversalMonoid[F] {

  def empty[A] = id[A]

  def unite[G[_], A](fga: F[G[A]])(implicit G: Foldable[G]): F[A] =
    flatMap(fga)(ga => G.foldMap(ga)(a => insert(a))(toMonoid[A]))
}

