package fundamentum

import simulacrum.typeclass

@typeclass trait Traverse[F[_]] extends Foldable[F] {

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, A](fa: F[G[A]]): G[F[A]] =
    traverse(fa)(identity)
}

@typeclass trait Traverse1[F[_]] extends Foldable1[F] {

  def traverse1[G[_]: Apply, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence1[G[_]: Apply, A](fa: F[G[A]]): G[F[A]] =
    traverse1(fa)(identity)
}
