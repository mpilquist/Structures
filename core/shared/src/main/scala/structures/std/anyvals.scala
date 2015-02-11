package structures
package std

trait anyvals {
  implicit val byteMonoid: Monoid[Byte] = Monoid.instance(0: Byte)((x, y) => (x + y).toByte)
  implicit val shortMonoid: Monoid[Short] = Monoid.instance(0: Short)((x, y) => (x + y).toShort)
  implicit val intMonoid: Monoid[Int] = Monoid.instance(0)(_ + _)
  implicit val longMonoid: Monoid[Long] = Monoid.instance(0L)(_ + _)
  implicit val stringMonoid: Monoid[String] = Monoid.instance("")(_ + _)

  implicit val byteEqual: Equal[Byte] = Equal.natural
  implicit val shortEqual: Equal[Short] = Equal.natural
  implicit val intEqual: Equal[Int] = Equal.natural
  implicit val longEqual: Equal[Long] = Equal.natural
  implicit val stringEqual: Equal[String] = Equal.natural

}

object anyvals extends anyvals
