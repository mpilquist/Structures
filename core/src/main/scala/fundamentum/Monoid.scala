package fundamentum

import simulacrum.typeclass

@typeclass trait Monoid[A] extends Semigroup[A] {
  def id: A
}
