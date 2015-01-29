package structures

import simulacrum.typeclass

@typeclass trait Foldable[F[_]] extends Functor[F] {

  def foldLeft[A, B](fa: F[A], initial: B)(f: (B, A) => B): B

  def foldRight[A, B](fa: F[A], initial: B)(f: (A, B) => B): B

  def foldMap[A, B](fa: F[A])(f: A => B)(implicit mb: Monoid[B]): B =
    foldLeft(fa, mb.id)((b, a) => mb.append(b, f(a)))

  def fold[A: Monoid](fa: F[A]): A =
    foldMap(fa)(identity)

  def traverse_[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[Unit] =
    foldLeft(fa, Applicative[G].pure(()))((acc, a) => Applicative[G].map2(acc, f(a))((_, _) => ()))

  def sequence_[G[_]: Applicative, A, B](fga: F[G[A]]): G[Unit] =
    traverse_(fga)(identity)

  def psum[G[_]: UMonoid, A](fga: F[G[A]]): G[A] =
    foldLeft(fga, UMonoid[G].id[A])((acc, ga) => UMonoid[G].append(acc, ga))
}

@typeclass trait Foldable1[F[_]] extends Foldable[F] {

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
}
