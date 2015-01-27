package structures

import simulacrum.{ noop, op, typeclass }

@typeclass trait FlatMap[F[_]] extends Apply[F] {

  @op(">>=", alias = true)
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def flatten[A, B](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(identity)

  @noop override def apply[A, B](fa: F[A])(f: F[A => B]): F[B] =
    flatMap(f)(map(fa))
}
