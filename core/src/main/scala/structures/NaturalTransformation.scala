package structures

import simulacrum.typeclass

trait NatrualTransformation[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}
