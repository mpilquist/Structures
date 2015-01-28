package structures

import simulacrum.{ typeclass, op }

@typeclass trait Semigroup[A] {
  @op("|+|") def append(x: A, y: => A): A
}
