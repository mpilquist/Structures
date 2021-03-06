package structures

import simulacrum.typeclass

@typeclass trait Monad[F[_]] extends Any with FlatMap[F] with Applicative[F] {

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))
}
