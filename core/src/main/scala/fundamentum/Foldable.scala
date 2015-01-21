package fundamentum

import simulacrum.typeclass

@typeclass trait Foldable[F[_]] extends Functor[F] {

  def foldRight[A, B](fa: F[A], initial: B)(f: (A, B) => B): B

  def foldMap[A, B](fa: F[A])(f: A => B)(implicit mb: Monoid[B]): B =
    foldRight(fa, mb.id)((a, b) => mb.append(f(a), b))

  def fold[A: Monoid](fa: F[A]): A =
    foldMap(fa)(x => x)
}

@typeclass trait Foldable1[F[_]] extends Foldable[F] {

  def foldRight1[A, B](fa: F[A])(initial: A => B)(f: (A, B) => B): B

  override def foldRight[A, B](fa: F[A], initial: B)(f: (A, B) => B): B =
    foldRight1(fa)(f(_, initial))(f)

  def foldMap1[A, B](fa: F[A])(f: A => B)(implicit sb: Semigroup[B]): B =
    foldRight1(fa)(f)((a, b) => sb.append(f(a), b))

  def fold1[A: Semigroup](fa: F[A]): A =
    foldMap1(fa)(x => x)

  override def foldMap[A, B](fa: F[A])(f: A => B)(implicit mb: Monoid[B]): B =
    foldMap1(fa)(f)(mb)
}
