package structures
package laws

trait ApplicativeLaws[F[_]] extends ApplyLaws[F] {

  implicit val typeClass: Applicative[F]

  import Applicative.ops._, typeClass.pure

  def applicativeIdentity[A](fa: F[A]): IsEqual[F[A]] =
    fa.apply(pure((a: A) => a)) =?= fa

  def applicativeComposition[A, B, C](fa: F[A], fab: F[A => B], fbc: F[B => C]): IsEqual[F[C]] =
    fa.apply(fab).apply(fbc) =?= fa.apply(fab.apply(fbc.apply(pure((bc: B => C) => (ab: A => B) => ab andThen bc))))

  def applicativeHomomorphism[A, B](a: A, f: A => B): IsEqual[F[B]] =
    pure(a).apply(pure(f)) =?= pure(f(a))

  def applicativeInterchange[A, B](a: A, fab: F[A => B]): IsEqual[F[B]] =
    pure(a).apply(fab) =?= fab.apply(pure((f: A => B) => f(a)))

  def applicativeMapConsistency[A, B](fa: F[A], f: A => B): IsEqual[F[B]] =
    fa.map(f) =?= fa.apply(pure(f))
}

object ApplicativeLaws {
  def apply[F[_]: Applicative]: ApplicativeLaws[F] = new ApplicativeLaws[F] {
    val typeClass = Applicative[F]
  }
}
