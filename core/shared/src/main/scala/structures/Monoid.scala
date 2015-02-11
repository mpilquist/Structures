package structures

import simulacrum.typeclass

@typeclass trait Monoid[A] extends Any with Semigroup[A] {
  def empty: A
}

object Monoid {

  def instance[A](empty: A)(append: (A, => A) => A): Monoid[A] = {
    val empty0 = empty
    val append0 = append
    new Monoid[A] {
      def empty = empty0
      def append(x: A, y: => A) = append0(x, y)
    }
  }

  implicit def fromUMonoid[F[_], A](implicit p: UMonoid[F]): Monoid[F[A]] = p.toMonoid
}
