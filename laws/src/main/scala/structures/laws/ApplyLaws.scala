package structures
package laws

trait ApplyLaws[F[_]] extends FunctorLaws[F] {

  implicit val typeClass: Apply[F]

  import Apply.ops._

  def applyAssociativeComposition[A, B, C](fa: F[A], fab: F[A => B], fbc: F[B => C]): IsEqual[F[C]] =
    fa.apply(fab).apply(fbc) =?= fa.apply(fab.apply(fbc.map((bc: B => C) => (ab: A => B) => ab andThen bc)))
}

object ApplyLaws {
  def apply[F[_]: Apply]: ApplyLaws[F] = new ApplyLaws[F] {
    val typeClass = Apply[F]
  }
}
