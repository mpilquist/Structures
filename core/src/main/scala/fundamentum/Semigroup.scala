package fundamentum

import simulacrum.typeclass

@typeclass trait Semigroup[A] {
  def append(x: A, y: A): A
}
