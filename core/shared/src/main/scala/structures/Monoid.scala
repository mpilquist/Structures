package structures

import simulacrum.typeclass

@typeclass trait Monoid[A] extends Any with Semigroup[A] {
  def empty: A
}

object Monoid {

  def instance[A](empty: A)(combine: (A, => A) => A): Monoid[A] = {
    val empty0 = empty
    val combine0 = combine
    new Monoid[A] {
      def empty = empty0
      def combine(x: A, y: => A) = combine0(x, y)
    }
  }
}
