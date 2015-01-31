package structures

import simulacrum.typeclass

@typeclass trait Traverse[F[_]] extends Any with Functor[F] with Foldable[F] { self =>

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, A](fa: F[G[A]]): G[F[A]] =
    traverse(fa)(identity)

  def compose[G[_]: Traverse]: Traverse[Lambda[X => F[G[X]]]] =
    new Traverse.Composite[F, G] {
      def F = self
      def G = Traverse[G]
    }
}

object Traverse {

  trait Composite[F[_], G[_]] extends Traverse[Lambda[X => F[G[X]]]] with Functor.Composite[F, G] with Foldable.Composite[F, G] {
    def F: Traverse[F]
    def G: Traverse[G]
    def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
      F.traverse(fga)(ga => G.traverse(ga)(f))
  }
}

@typeclass trait Traverse1[F[_]] extends Any with Functor[F] with Foldable1[F] { self =>

  def traverse1[G[_]: Apply, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence1[G[_]: Apply, A](fa: F[G[A]]): G[F[A]] =
    traverse1(fa)(identity)

  def compose[G[_]: Traverse1]: Traverse1[Lambda[X => F[G[X]]]] =
    new Traverse1.Composite[F, G] {
      def F = self
      def G = Traverse1[G]
    }
}

object Traverse1 {

  trait Composite[F[_], G[_]] extends Traverse1[Lambda[X => F[G[X]]]] with Functor.Composite[F, G] with Foldable1.Composite[F, G] {
    def F: Traverse1[F]
    def G: Traverse1[G]
    def traverse1[H[_]: Apply, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
      F.traverse1(fga)(ga => G.traverse1(ga)(f))
  }
}
