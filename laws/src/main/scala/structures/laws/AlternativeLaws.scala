package structures
package laws

trait AlternativeLaws[F[_]] extends ApplicativeLaws[F] with MonoidKLaws[F] {

  implicit val typeClass: Alternative[F]

  import Alternative.ops._
  import typeClass.{ pure, empty }

  def alternativeRightDistributivity[A, B](fa: F[A], ff: F[A => B], fg: F[A => B]): IsEqual[F[B]] =
    fa.apply(ff |+| fg) =?= (fa.apply(ff) |+| fa.apply(fg))

  def alternativeRightAbsorption[A, B](ff: F[A => B]): IsEqual[F[B]] =
    empty[A].apply(ff) =?= empty[B]

  def alternativeLeftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => B): IsEqual[F[B]] =
    (fa |+| fa2).map(f) =?= ((fa map f) |+| (fa2 map f))
}

object AlternativeLaws {
  def apply[F[_]: Alternative]: AlternativeLaws[F] = new AlternativeLaws[F] {
    val typeClass = Alternative[F]
  }
}
