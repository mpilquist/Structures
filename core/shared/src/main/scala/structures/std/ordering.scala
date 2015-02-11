package structures
package std

trait ordering {

  implicit val ordering: Contravariant[Ordering] = new Contravariant[Ordering] {
    def contramap[A, B](fa: Ordering[A])(f: B => A): Ordering[B] =
      fa.on(f)
  }
}

object ordering extends ordering
