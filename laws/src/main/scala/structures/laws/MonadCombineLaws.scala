package structures
package laws

trait MonadCombineLaws[F[_]] extends MonadFilterLaws[F] with AlternativeLaws[F] {

  implicit val typeClass: MonadCombine[F]

  import MonadCombine.ops._

  def monadCombineLeftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => F[B]): IsEqual[F[B]] =
    (fa |+| fa2).flatMap(f) =?= ((fa flatMap f) |+| (fa2 flatMap f))
}

object MonadCombineLaws {
  def apply[F[_]: MonadCombine]: MonadCombineLaws[F] = new MonadCombineLaws[F] {
    val typeClass = MonadCombine[F]
  }
}
