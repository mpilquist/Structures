package fundamentum

import simulacrum.typeclass

@typeclass trait Extract[F[_]] extends Extend[F] {
  def extract[A](a: F[A]): A
}
