package structures

import simulacrum.{ typeclass, op }

@typeclass trait Equal[A] {

  @op("===")
  def equal(x: A, y: A): Boolean

  @op("=!=")
  final def notEqual(x: A, y: A): Boolean = !equal(x, y)
}

object Equal {

  def natural[A]: Equal[A] = new Equal[A] {
    def equal(x: A, y: A) = x == y
  }

  def instance[A](f: (A, A) => Boolean): Equal[A] = new Equal[A] {
    def equal(x: A, y: A) = f(x, y)
  }
}
