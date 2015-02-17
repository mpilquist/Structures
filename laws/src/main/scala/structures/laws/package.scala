package structures

package object laws {

  implicit class IsEqualSyntax[A](val lhs: A) extends AnyVal {
    def =?=(rhs: A): IsEqual[A] = IsEqual(lhs, rhs)
  }
}
