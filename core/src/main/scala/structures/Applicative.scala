package structures

import simulacrum.typeclass

/**
 * Type class that describes functors which have a lawful `Apply` instance and
 * that support a `pure` method which adheres to the laws described in
 * [[structures.laws.ApplicativeLaws]].
 *
 * Also known as idioms.
 *
 * @see http://strictlypositive.org/IdiomLite.pdf
 */
@typeclass trait Applicative[F[_]] extends Any with Apply[F] with Functor[F] {

  def pure[A](a: A): F[A]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(fa)(pure(f))
}
