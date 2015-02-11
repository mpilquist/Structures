package structures

import simulacrum.typeclass

@typeclass trait Alternative[F[_]] extends Any with Applicative[F] with UMonoid[F] { self =>

  def asum[G[_]: Foldable, A](gfa: G[F[A]]): F[A] =
    Foldable[G].fold(gfa)(toMonoid[A])

  def compose[G[_]: Alternative]: Alternative[Lambda[X => F[G[X]]]] =
    new Alternative.Composite[F, G] {
      def F = self
      def G = Alternative[G]
    }
}

object Alternative {

  trait Composite[F[_], G[_]] extends Any with Alternative[Lambda[X => F[G[X]]]] with Applicative.Composite[F, G] {
    def F: Alternative[F]
    def G: Alternative[G]
    def empty[A]: F[G[A]] = F.empty
    def append[A](x: F[G[A]], y: => F[G[A]]): F[G[A]] = F.append(x, y)
  }
}
