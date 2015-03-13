package structures
package std

trait either {

  implicit def eitherTraverse[L]: Traverse[Either[L, ?]] = new Traverse[Either[L, ?]] {
    def map[A, B](fa: Either[L, A])(f: A => B) = fa.right.map(f)
    def foldLeft[A, B](fa: Either[L, A], initial: B)(f: (B, A) => B) = fa.fold(_ => initial, a => f(initial, a))
    def foldRight[A, B](fa: Either[L, A], initial: B)(f: (A, B) => B) = fa.fold(_ => initial, a => f(a, initial))
    def traverse[G[_]: Applicative, A, B](fa: Either[L, A])(f: A => G[B]): G[Either[L, B]] = {
      fa.right.map(f).fold(l => Applicative[G].pure(Left(l)), gb => Applicative[G].map(gb)(b => Right(b)))
    }
  }

  implicit def eitherMonad[L]: Monad[Either[L, ?]] = new Monad[Either[L, ?]] {
    def pure[A](a: A) = Right(a)
    def flatMap[A, B](fa: Either[L, A])(f: A => Either[L, B]) = fa.right.flatMap(f)
  }

  def accumulatingEither[L](implicit L: Semigroup[L]): Applicative[Either[L, ?]] = new Applicative[Either[L, ?]] {
    def pure[A](a: A) = Right(a)
    def apply[A, B](fa: Either[L, A])(ff: Either[L, A => B]) = (fa, ff) match {
      case (Right(a), Right(f)) => Right(f(a))
      case (l, Right(_)) => l.asInstanceOf[Either[L, B]]
      case (Right(_), l) => l.asInstanceOf[Either[L, B]]
      case (Left(x), Left(y)) => Left(L.combine(x, y))
    }
  }
}

object either extends either

