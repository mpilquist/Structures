package structures

import simulacrum.{ typeclass, op }

@typeclass trait Semigroup[A] extends Any {
  @op("|+|") def combine(x: A, y: => A): A
}

object Semigroup {

  def instance[A](combine: (A, => A) => A): Semigroup[A] = {
    val combine0 = combine
    new Semigroup[A] {
      def combine(x: A, y: => A) = combine0(x, y)
    }
  }

  implicit def fromSemigroupK[F[_], A](implicit p: SemigroupK[F]): Semigroup[F[A]] = p.toSemigroup
}
