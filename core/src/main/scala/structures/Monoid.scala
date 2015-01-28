package structures

import simulacrum.typeclass

@typeclass trait Monoid[A] extends Semigroup[A] {
  def id: A
}

object Monoid {

  implicit def fromPlusEmpty[F[_], A](implicit p: PlusEmpty[F]): Monoid[F[A]] = p.toMonoid
}
