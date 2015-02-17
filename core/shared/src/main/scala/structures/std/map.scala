package structures
package std

trait map {

  implicit def mapMonoid[K, V: Semigroup]: Monoid[Map[K, V]] = Monoid.instance(Map.empty[K, V]) { (x, y) =>
    y.foldLeft(x) { case (acc, (k, v)) => acc.updated(k, (acc.get(k).fold(v)(xv => Semigroup[V].combine(xv, v)))) }
  }

  implicit def mapEqual[K, V: Equal]: Equal[Map[K, V]] = Equal.instance((x, y) =>
    x.size == y.size && {
      x.forall { case (xk, xv) =>
        y.get(xk).fold(false)(yv => Equal[V].equal(xv, yv))
      }
    }
  )

  implicit def map[K]: FlatMap[Map[K, ?]] = new FlatMap[Map[K, ?]] {
    override def map[A, B](fa: Map[K, A])(f: A => B) =
      fa.map { case (k, v) => (k, f(v)) }
    override def apply[A, B](fa: Map[K, A])(f: Map[K, A => B]) =
      fa.flatMap { case (k, v) => f.get(k).map { fab => Map(k -> fab(v)) }.getOrElse(Map.empty) }
    def flatMap[A, B](fa: Map[K, A])(f: A => Map[K, B]): Map[K, B] =
      fa.flatMap { case (k, v) => f(v).get(k).fold(Map.empty[K, B])(b => Map(k -> b)) }
  }
}

object map extends map

