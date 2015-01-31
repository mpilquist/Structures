package structures

import simulacrum.typeclass

@typeclass trait Foldable[F[_]] extends Any { self =>

  def foldLeft[A, B](fa: F[A], initial: B)(f: (B, A) => B): B

  def foldRight[A, B](fa: F[A], initial: B)(f: (A, B) => B): B

  def foldMap[A, B](fa: F[A])(f: A => B)(implicit mb: Monoid[B]): B =
    foldLeft(fa, mb.empty)((b, a) => mb.append(b, f(a)))

  def fold[A: Monoid](fa: F[A]): A =
    foldMap(fa)(identity)

  def traverse_[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[Unit] =
    foldLeft(fa, Applicative[G].pure(()))((acc, a) => Applicative[G].map2(acc, f(a))((_, _) => ()))

  def sequence_[G[_]: Applicative, A, B](fga: F[G[A]]): G[Unit] =
    traverse_(fga)(identity)

  def psum[G[_]: UMonoid, A](fga: F[G[A]]): G[A] =
    foldLeft(fga, UMonoid[G].empty[A])((acc, ga) => UMonoid[G].append(acc, ga))

  def compose[G[_]: Foldable]: Foldable[Lambda[X => F[G[X]]]] =
    new Foldable.Composite[F, G] {
      def F = self
      def G = Foldable[G]
    }
}

object Foldable {

  trait Composite[F[_], G[_]] extends Foldable[Lambda[X => F[G[X]]]] {
    def F: Foldable[F]
    def G: Foldable[G]
    def foldLeft[A, B](fa: F[G[A]], initial: B)(f: (B, A) => B): B =
      F.foldLeft(fa, initial)((acc, ga) => G.foldLeft(ga, acc)(f))
    def foldRight[A, B](fa: F[G[A]], initial: B)(f: (A, B) => B): B =
      F.foldRight(fa, initial)((ga, acc) => G.foldRight(ga, acc)(f))
  }
}

@typeclass trait Foldable1[F[_]] extends Any with Foldable[F] { self =>

  def foldLeft1[A, B](fa: F[A])(initial: A => B)(f: (B, A) => B): B

  def foldRight1[A, B](fa: F[A])(initial: A => B)(f: (A, B) => B): B

  override def foldLeft[A, B](fa: F[A], initial: B)(f: (B, A) => B): B =
    foldLeft1(fa)(f(initial, _))(f)

  override def foldRight[A, B](fa: F[A], initial: B)(f: (A, B) => B): B =
    foldRight1(fa)(f(_, initial))(f)

  def foldMap1[A, B](fa: F[A])(f: A => B)(implicit sb: Semigroup[B]): B =
    foldLeft1(fa)(f)((b, a) => sb.append(b, f(a)))

  def fold1[A: Semigroup](fa: F[A]): A =
    foldMap1(fa)(identity)

  def compose[G[_]: Foldable1]: Foldable1[Lambda[X => F[G[X]]]] =
    new Foldable1.Composite[F, G] {
      def F = self
      def G = Foldable1[G]
    }
}

object Foldable1 {

  trait Composite[F[_], G[_]] extends Foldable1[Lambda[X => F[G[X]]]] {
    def F: Foldable1[F]
    def G: Foldable1[G]
    def foldLeft1[A, B](fga: F[G[A]])(initial: A => B)(f: (B, A) => B): B =
      F.foldLeft1(fga)(G.foldLeft1(_)(initial)(f))((acc, ga) => G.foldLeft(ga, acc)(f))
    def foldRight1[A, B](fga: F[G[A]])(initial: A => B)(f: (A, B) => B): B =
      F.foldRight1(fga)(G.foldRight1(_)(initial)(f))((ga, acc) => G.foldRight(ga, acc)(f))
  }
}
