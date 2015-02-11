package structures

import simulacrum.typeclass

@typeclass trait Extract[F[_]] extends Any with Extend[F] {
  def extract[A](a: F[A]): A
}
