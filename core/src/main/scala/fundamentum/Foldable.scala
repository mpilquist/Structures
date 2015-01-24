package fundamentum

import simulacrum.typeclass

@typeclass trait Foldable[F[_]] extends Functor[F] {

  def foldLeft[A, B](fa: F[A], initial: B)(f: (B, A) => B): B

  def foldRight[A, B](fa: F[A], initial: B)(f: (A, B) => B): B
}

@typeclass trait Foldable1[F[_]] extends Foldable[F] {

  def foldLeft1[A, B](fa: F[A])(initial: A => B)(f: (B, A) => B): B
  def foldRight1[A, B](fa: F[A])(initial: A => B)(f: (A, B) => B): B

  override def foldLeft[A, B](fa: F[A], initial: B)(f: (B, A) => B): B =
    foldLeft1(fa)(f(initial, _))(f)

  override def foldRight[A, B](fa: F[A], initial: B)(f: (A, B) => B): B =
    foldRight1(fa)(f(_, initial))(f)
}
