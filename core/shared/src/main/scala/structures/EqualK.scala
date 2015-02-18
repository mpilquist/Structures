package structures

import simulacrum._

@typeclass trait EqualK[F[_]] {

  @op("===")
  def equal[A: Equal](x: F[A], y: F[A]): Boolean

  @op("=!=")
  final def notEqual[A: Equal](x: F[A], y: F[A]): Boolean = !equal(x, y)

  final def toEqual[A: Equal]: Equal[F[A]] = Equal.instance((x, y) => equal(x, y))
}
