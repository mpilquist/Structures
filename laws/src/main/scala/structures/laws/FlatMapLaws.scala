package structures
package laws

trait FlatMapLaws[F[_]] extends ApplyLaws[F] {

  implicit val typeClass: FlatMap[F]

  import FlatMap.ops._

  def flatMapAssociativity[A, B, C](fa: F[A], f: A => F[B], g: B => F[C]): IsEqual[F[C]] =
    fa.flatMap(f).flatMap(g) =?= fa.flatMap { a => f(a).flatMap(g) }
}

object FlatMapLaws {
  def apply[F[_]: FlatMap]: FlatMapLaws[F] = new FlatMapLaws[F] {
    val typeClass = FlatMap[F]
  }
}
