package structures

package object std {

  implicit val byteMonoid: Monoid[Byte] = Monoid.instance(0: Byte)((x, y) => (x + y).toByte)
  implicit val shortMonoid: Monoid[Short] = Monoid.instance(0: Short)((x, y) => (x + y).toShort)
  implicit val intMonoid: Monoid[Int] = Monoid.instance(0)(_ + _)
  implicit val longMonoid: Monoid[Long] = Monoid.instance(0L)(_ + _)
  implicit val stringMonoid: Monoid[String] = Monoid.instance("")(_ + _)

  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = Monoid.instance(None: Option[A])((x, y) =>
    x.fold(y)(xx => y.fold(Some(xx))(yy => Some(Semigroup[A].append(xx, yy)))))

  implicit val option: MonadAppend[Option] with Traverse[Option] = new MonadAppend[Option] with Traverse[Option] {
    def pure[A](a: A) = Some(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]) = fa flatMap f
    def id[A] = None
    def append[A](fa: Option[A], fb: => Option[A]) = fa // invalid definition, let tests drive this fix though
    def foldLeft[A, B](fa: Option[A], initial: B)(f: (B, A) => B) = fa.foldLeft(initial)(f)
    def foldRight[A, B](fa: Option[A], initial: B)(f: (A, B) => B) = fa.fold(initial)(f(_, initial))
    def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = {
      fa.fold(Applicative[G].pure(None: Option[B])) { a =>
        Applicative[G].map(f(a))(pure)
      }
    }
  }

  implicit val list: MonadAppend[List] with Traverse[List] = new MonadAppend[List] with Traverse[List] {
    def pure[A](a: A) = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]) = fa flatMap f
    def id[A] = Nil
    def append[A](x: List[A], y: => List[A]) = x ++ y
    def foldLeft[A, B](fa: List[A], initial: B)(f: (B, A) => B) = fa.foldLeft(initial)(f)
    def foldRight[A, B](fa: List[A], initial: B)(f: (A, B) => B) = fa.foldRight(initial)(f)
    def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
      fa.reverse.foldLeft(Applicative[G].pure(Nil: List[B])) { (acc, a) =>
        Applicative[G].map2(f(a), acc)(_ :: _)
      }
    }
  }

  implicit def mapMonoid[K, V: Semigroup]: Monoid[Map[K, V]] = Monoid.instance(Map.empty[K, V]) { (x, y) =>
    y.foldLeft(x) { case (acc, (k, v)) => acc.updated(k, (acc.get(k).fold(v)(xv => Semigroup[V].append(xv, v)))) }
  }

  implicit def map[K]: Apply[Map[K, ?]] = new Apply[Map[K, ?]] {
    def map[A, B](fa: Map[K, A])(f: A => B) = fa.map { case (k, v) => (k, f(v)) }
    def apply[A, B](fa: Map[K, A])(f: Map[K, A => B]) = fa.flatMap { case (k, v) => f.get(k).map { fab => Map(k -> fab(v)) }.getOrElse(Map.empty) }
  }

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
