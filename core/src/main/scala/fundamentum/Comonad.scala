package fundamentum

import simulacrum.typeclass

@typeclass trait Comonad[F[_]] extends Extendable[F] {
  def extract[A](a: F[A]): A
}
