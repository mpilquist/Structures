package structures
package std

trait option {

  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = Monoid.instance(None: Option[A])((x, y) =>
    x.fold(y)(xx => y.fold(Some(xx))(yy => Some(Semigroup[A].combine(xx, yy)))))

  implicit val optionEqualK: EqualK[Option] = new EqualK[Option] {
    def equal[A](x: Option[A], y: Option[A])(implicit A: Equal[A]) = (x, y) match {
      case (Some(x), Some(y)) => A.equal(x, y)
      case (None, None) => true
      case _ => false
    }
  }

  implicit val option: MonadCombine[Option] with Traverse[Option] = new MonadCombine[Option] with Traverse[Option] {
    def pure[A](a: A) = Some(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]) = fa flatMap f
    def empty[A] = None
    def combine[A](fa: Option[A], fb: => Option[A]) = fa orElse fb
    def foldLeft[A, B](fa: Option[A], initial: B)(f: (B, A) => B) = fa.foldLeft(initial)(f)
    def foldRight[A, B](fa: Option[A], initial: B)(f: (A, B) => B) = fa.fold(initial)(f(_, initial))
    def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = {
      fa.fold(Applicative[G].pure(None: Option[B])) { a =>
        Applicative[G].map(f(a))(pure)
      }
    }
  }


}

object option extends option
