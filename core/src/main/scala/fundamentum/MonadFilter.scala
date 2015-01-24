package fundamentum

import simulacrum.typeclass

/**
 * Monad that defines one additional abstract method, `empty`, such that:
 *  - for all `A`, `fa: F[A]`, `flatMap(empty)(_ => fa) == empty`
 *
 * This gives rise the the `filter` method and its variants.
 */
@typeclass trait MonadFilter[F[_]] extends Monad[F] {

  def empty[A]: F[A]

  def filter[A](fa: F[A])(f: A => Boolean) =
    flatMap(fa)(a => if (f(a)) insert(a) else empty[A])

  def filterM[A](fa: F[A])(f: A => F[Boolean]) =
    flatMap(fa)(a => flatMap(f(a))(b => if (b) insert(a) else empty[A]))
}

