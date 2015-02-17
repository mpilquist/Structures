package structures

import simulacrum.{ typeclass, op }

@typeclass trait EqualK[F[_]] {

  def equal[A: Equal](x: F[A], y: F[A]): Boolean

  def toEqual[A: Equal]: Equal[F[A]] =
    Equal.instance(equal[A])
}
