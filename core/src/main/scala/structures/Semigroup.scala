package structures

import simulacrum.{ typeclass, op }

@typeclass trait Semigroup[A] {
  @op("|+|") def append(x: A, y: => A): A
}

object Semigroup {

  implicit def fromPlus[F[_], A](implicit p: Plus[F]): Semigroup[F[A]] = p.toSemigroup
}
