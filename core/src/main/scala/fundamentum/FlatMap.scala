package fundamentum

import simulacrum.{ op, typeclass }

@typeclass trait FlatMap[F[_]] extends Apply[F] {
  @op(">>=", alias = true)
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  override def apply[A, B](fa: F[A])(f: F[A => B]): F[B] =
    flatMap(f)(map(fa))
}
