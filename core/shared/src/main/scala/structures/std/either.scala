package structures
package std

trait either {

  implicit def either[L]: Monad[Either[L, ?]] with Traverse[Either[L, ?]] = new Monad[Either[L, ?]] with Traverse[Either[L, ?]] {
    def pure[A](a: A) = Right(a)
    def flatMap[A, B](fa: Either[L, A])(f: A => Either[L, B]) = fa.right.flatMap(f)
    def foldLeft[A, B](fa: Either[L, A], initial: B)(f: (B, A) => B) = fa.fold(_ => initial, a => f(initial, a))
    def foldRight[A, B](fa: Either[L, A], initial: B)(f: (A, B) => B) = fa.fold(_ => initial, a => f(a, initial))
    def traverse[G[_]: Applicative, A, B](fa: Either[L, A])(f: A => G[B]): G[Either[L, B]] = {
      fa.right.map(f).fold(l => Applicative[G].pure(Left(l)), gb => Applicative[G].map(gb)(b => Right(b)))
    }
  }
}

object either extends either

