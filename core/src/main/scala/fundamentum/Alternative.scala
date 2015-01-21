package fundamentum

import simulacrum.typeclass

@typeclass trait Alternative[F[_]] extends Applicative[F] with UniversalMonoid[F] {

}


