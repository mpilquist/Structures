package fundamentum

import simulacrum.typeclass

@typeclass trait UniversalMonoid[F[_]] extends UniversalSemigroup[F] { self =>
  def id[A]: F[A]

  def toMonoid[A]: Monoid[F[A]] = new Monoid[F[A]] {
    def append(x: F[A], y: F[A]): F[A] = self.append(x, y)
    def id = self.id
  }
}
