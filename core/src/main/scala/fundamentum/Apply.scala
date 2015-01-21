package fundamentum

import simulacrum.typeclass

@typeclass trait Apply[F[_]] extends Functor[F] {
  def apply[A, B](fa: F[A])(f: F[A => B]): F[B]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(fa)(map(fb)(b => (a: A) => f(a, b)))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(fa)(map2(fb, fc)((b, c) => a => f(a, b, c)))

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(fa)(map3(fb, fc, fd)((b, c, d) => a => f(a, b, c, d)))

  def map5[A, B, C, D, E, G](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])(f: (A, B, C, D, E) => G): F[G] =
    apply(fa)(map4(fb, fc, fd, fe)((b, c, d, e) => a => f(a, b, c, d, e)))
}
