package structures
package laws

trait MonadFilterLaws[F[_]] extends MonadLaws[F] {

  implicit val typeClass: MonadFilter[F]

  import MonadFilter.ops._, typeClass.empty

  def monadFilterLeftDistributivity[A, B](f: A => F[B]): IsEqual[F[B]] =
    empty[A].flatMap(f) =?= empty[B]

  def monadFilterRightDistributivity[A](fa: F[A]): IsEqual[F[A]] =
    fa.flatMap { a => empty[A] } =?= empty[A]
}

object MonadFilterLaws {
  def apply[F[_]: MonadFilter]: MonadFilterLaws[F] = new MonadFilterLaws[F] {
    val typeClass = MonadFilter[F]
  }
}
