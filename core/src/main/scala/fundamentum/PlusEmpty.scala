package fundamentum

import simulacrum.typeclass

@typeclass trait PlusEmpty[F[_]] extends Plus[F] { self =>
  def empty[A]: F[A]
}
