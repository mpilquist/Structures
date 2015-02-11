package structures
package std

trait function {

  implicit def function1Monad[I]: Monad[I => ?] = new Monad[I => ?] {
    def pure[A](a: A): I => A = _ => a
    override def map[A, B](fa: I => A)(f: A => B): I => B = fa andThen f
    def flatMap[A, B](fa: I => A)(f: A => (I => B)): I => B =
      i => f(fa(i))(i)
  }

  implicit def function1Contravariant[O]: Contravariant[? => O] = new Contravariant[? => O] {
    def contramap[A, B](fa: A => O)(f: B => A): B => O = f andThen fa
  }
}

object function extends function
