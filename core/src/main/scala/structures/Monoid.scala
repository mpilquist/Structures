package structures

import simulacrum.typeclass

@typeclass trait Monoid[A] extends Semigroup[A] {
  def id: A
}

object Monoid {

  def instance[A](id: A)(append: (A, => A) => A): Monoid[A] = {
    val id0 = id
    val append0 = append
    new Monoid[A] {
      def id = id0
      def append(x: A, y: => A) = append0(x, y)
    }
  }

  implicit def fromUMonoid[F[_], A](implicit p: UMonoid[F]): Monoid[F[A]] = p.toMonoid
}
