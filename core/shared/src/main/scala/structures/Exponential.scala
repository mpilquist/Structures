package structures

import simulacrum.typeclass

/**
 * Type class that describes type constructors that support an `xmap` method which adheres
 * to the laws described in `structures.laws.ExponentialLaws`.
 *
 * The name is short for "exponential functor", which is also known as "invariant functor".
 */
@typeclass trait Exponential[F[_]] extends Any { self =>

  /**
   * Converts the supplied `F[A]` to an `F[B]` using a pair of functions -- `A => B` and `B => A`.
   */
  def xmap[A, B](fa: F[A])(f: A => B, g: B => A): F[B]

  def compose[G[_]: Exponential]: Exponential[Lambda[X => F[G[X]]]] =
    new Exponential.Composite[F, G] {
      def F = self
      def G = Exponential[G]
    }

  def composeWithFunctor[G[_]: Functor]: Exponential[Lambda[X => F[G[X]]]] =
    new Exponential.Composite[F, G] {
      def F = self
      def G = Functor[G]
    }

  def composeWithContravariant[G[_]: Contravariant]: Exponential[Lambda[X => F[G[X]]]] =
    new Exponential.Composite[F, G] {
      def F = self
      def G = Contravariant[G]
    }
}

object Exponential {

  trait Composite[F[_], G[_]] extends Any with Exponential[Lambda[X => F[G[X]]]] {
    def F: Exponential[F]
    def G: Exponential[G]
    def xmap[A, B](fga: F[G[A]])(f: A => B, g: B => A): F[G[B]] =
      F.xmap(fga)(ga => G.xmap(ga)(f, g), gb => G.xmap(gb)(g, f))
  }

  trait CovariantComposite[F[_], G[_]] extends Any with Exponential[Lambda[X => F[G[X]]]] {
    def F: Exponential[F]
    def G: Functor[G]
    def xmap[A, B](fga: F[G[A]])(f: A => B, g: B => A): F[G[B]] =
      F.xmap(fga)(ga => G.map(ga)(f), gb => G.map(gb)(g))
  }

  trait ContravariantComposite[F[_], G[_]] extends Any with Exponential[Lambda[X => F[G[X]]]] {
    def F: Exponential[F]
    def G: Contravariant[G]
    def xmap[A, B](fga: F[G[A]])(f: A => B, g: B => A): F[G[B]] =
      F.xmap(fga)(ga => G.contramap(ga)(g), gb => G.contramap(gb)(f))
  }
}
