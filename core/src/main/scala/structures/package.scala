package object structures {

  type ~>[F[_], G[_]] = NatrualTransformation[F, G]

  type Id[A] = A

  implicit val IdInstance: Monad[Id] = new Monad[Id] with Extract[Id] {
    def pure[A](a: A) = a
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]) = f(fa)
    def extract[A](a: Id[A]) = a
    def extend[A, B](fa: Id[A])(f: Id[A] => B) = f(fa)
  }
}
