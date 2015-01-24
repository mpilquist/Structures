package fundamentum

package object instances {

  implicit val list: MonadPlus[List] with Traverse[List] = new MonadPlus[List] with Traverse[List] {
    def insert[A](a: A) = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]) = fa flatMap f
    def empty[A] = Nil
    def plus[A](x: List[A], y: => List[A]) = x ++ y
    def foldLeft[A, B](fa: List[A], initial: B)(f: (B, A) => B) = fa.foldLeft(initial)(f)
    def foldRight[A, B](fa: List[A], initial: B)(f: (A, B) => B) = fa.foldRight(initial)(f)
    def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
      fa.reverse.foldLeft(Applicative[G].insert(Nil: List[B])) { (acc, a) =>
        Applicative[G].map2(f(a), acc)(_ :: _)
      }
    }
  }

  implicit val option: MonadPlus[Option] with Traverse[Option] = new MonadPlus[Option] with Traverse[Option] {
    def insert[A](a: A) = Some(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]) = fa flatMap f
    def empty[A] = None
    def plus[A](fa: Option[A], fb: => Option[A]) = fa
    def foldLeft[A, B](fa: Option[A], initial: B)(f: (B, A) => B) = fa.foldLeft(initial)(f)
    def foldRight[A, B](fa: Option[A], initial: B)(f: (A, B) => B) = fa.fold(initial)(f(_, initial))
    def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = {
      fa.fold(Applicative[G].insert(None: Option[B])) { a =>
        Applicative[G].map(f(a))(insert)
      }
    }
  }
}
