package structures
package laws

trait ExponentialLaws[F[_]] {

  implicit val typeClass: Exponential[F]

  import Exponential.ops._

  def exponentialIdentity[A](fa: F[A]): IsEqual[F[A]] =
    fa.xmap[A](identity, identity) =?= fa

  def exponentialComposition[A, B, C](fa: F[A], f1: A => B, f2: B => A, g1: B => C, g2: C => B): IsEqual[F[C]] =
    fa.xmap(f1, f2).xmap(g1, g2) =?= fa.xmap(f1 andThen g1, g2 andThen f2)
}

object ExponentialLaws {
  def apply[F[_]: Exponential]: ExponentialLaws[F] = new ExponentialLaws[F] {
    val typeClass = Exponential[F]
  }
}
