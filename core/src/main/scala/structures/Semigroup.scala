package structures

import simulacrum.{ typeclass, op }

@typeclass trait Semigroup[A] {
  @op("|+|") def append(x: A, y: => A): A
}

object Semigroup {

  def instance[A](append: (A, => A) => A): Semigroup[A] = {
    val append0 = append
    new Semigroup[A] {
      def append(x: A, y: => A) = append0(x, y)
    }
  }

  implicit def fromPlus[F[_], A](implicit p: Plus[F]): Semigroup[F[A]] = p.toSemigroup
}
