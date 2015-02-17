package structures
package laws

case class IsEqual[A](lhs: A, rhs: A) {
  def isEqual(implicit eq: Equal[A]): Boolean = eq.equal(lhs, rhs)
}
