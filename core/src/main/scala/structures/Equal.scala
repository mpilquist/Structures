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

  implicit val contravariantInstance: Contravariant[Equal] = new Contravariant[Equal] {
    def contramap[A, B](fa: Equal[A])(f: B => A): Equal[B] =
      Equal.instance[B]((x, y) => fa.equal(f(x), f(y)))
  }
}
