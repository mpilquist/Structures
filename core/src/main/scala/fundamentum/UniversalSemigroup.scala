package fundamentum

import simulacrum.typeclass

@typeclass trait UniversalSemigroup[F[_]] { self =>
  def append[A](x: F[A], y: F[A]): F[A]

  def toSemigroup[A]: Semigroup[F[A]] = new Semigroup[F[A]] {
    def append(x: F[A], y: F[A]): F[A] = self.append(x, y)
  }
}
