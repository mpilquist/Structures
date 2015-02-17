package structures
package std

trait list {

  implicit def listEqual[A](implicit A: Equal[A]): Equal[List[A]] = new Equal[List[A]] {
    def equal(x: List[A], y: List[A]) = {
      x.size == y.size && {
        x.zip(y).forall { case (xx, yy) => A.equal(xx, yy) }
      }
    }
  }

  implicit val list: MonadCombine[List] with Traverse[List] = new MonadCombine[List] with Traverse[List] {
    def pure[A](a: A) = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]) = fa flatMap f
    def empty[A] = Nil
    def combine[A](x: List[A], y: => List[A]) = x ++ y
    def foldLeft[A, B](fa: List[A], initial: B)(f: (B, A) => B) = fa.foldLeft(initial)(f)
    def foldRight[A, B](fa: List[A], initial: B)(f: (A, B) => B) = fa.foldRight(initial)(f)
    def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
      fa.reverse.foldLeft(Applicative[G].pure(Nil: List[B])) { (acc, a) =>
        Applicative[G].map2(f(a), acc)(_ :: _)
      }
    }
  }
}

object list extends list

