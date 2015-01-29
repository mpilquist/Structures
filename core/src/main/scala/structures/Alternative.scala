package structures

import simulacrum.typeclass

@typeclass trait Alternative[F[_]] extends Applicative[F] with UMonoid[F] {

  def asum[G[_]: Foldable, A](gfa: G[F[A]]): F[A] =
    Foldable[G].fold(gfa)(toMonoid[A])
}


