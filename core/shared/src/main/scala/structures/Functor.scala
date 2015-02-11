package structures

import simulacrum.typeclass

/**
 * Type class that describes type constructors that support a `map`
 * method which adheres to the laws described in
 * [[structures.laws.FunctorLaws]].
 *
 * The name is short for "covariant functor".
 *
 * Note that every functor is an exponential functor, where `xmap` is
 * implemented in terms of `map` by ignoring the `B => A` function.
 */
@typeclass trait Functor[F[_]] extends Any with Exponential[F] { self =>

  /**
   * Converts the supplied `F[A]` in to an `F[B]` using the supplied `A => B`.
   */
  def map[A, B](fa: F[A])(f: A => B): F[B]

  override def xmap[A, B](fa: F[A])(f: A => B, g: B => A): F[B] =
    map(fa)(f)

  /** empty the fa of the values, preserving the structure */
  def void[A](fa: F[A]): F[Unit] = map(fa)(_ => ())

  /** Lifts the supplied function in to the `F` type constructor. */
  def lift[A, B](f: A => B): F[A] => F[B] =
    fa => map(fa)(f)

  /** Replaces the `A` value in `F[A]` with the supplied value. */
  def as[A, B](fa: F[A], b: B): F[B] =
    map(fa)(_ => b)

  /**
   * Maps the supplied function over `F[A]`, returning the original value
   * and the result of the function application.
   */
  def zipWith[A, B](fa: F[A])(f: A => B): F[(A, B)] =
    map(fa)(a => (a, f(a)))

  /**
    * Compose this functor F with a functor G to produce a composite
    * Functor on G[F[_]], with a map method which uses an A => B to
    * map a G[F[A]] to a G[F[B]].
    */
  def compose[G[_]: Functor]: Functor[Lambda[X => F[G[X]]]] =
    new Functor.Composite[F, G] {
      def F = self
      def G = Functor[G]
    }

  override def composeWithFunctor[G[_]: Functor]: Functor[Lambda[X => F[G[X]]]] =
    compose[G]

  override def composeWithContravariant[G[_]: Contravariant]: Contravariant[Lambda[X => F[G[X]]]] =
    new Functor.ContravariantComposite[F, G] {
      def F = self
      def G = Contravariant[G]
    }
}

object Functor {

  trait Composite[F[_], G[_]] extends Any with Functor[Lambda[X => F[G[X]]]] {
    def F: Functor[F]
    def G: Functor[G]
    override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] =
      F.map(fa)(G.lift(f))
  }

  trait ContravariantComposite[F[_], G[_]] extends Any with Contravariant[Lambda[X => F[G[X]]]] {
    def F: Functor[F]
    def G: Contravariant[G]
    override def contramap[A, B](fa: F[G[A]])(f: B => A): F[G[B]] =
      F.map(fa)(ga => G.contramap(ga)(f))
  }
}
