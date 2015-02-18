package structures
package laws
package discipline

import org.scalacheck.Arbitrary
import Arbitrary.arbitrary

trait ArbitraryK[F[_]] {
  def toArbitrary[A: Arbitrary]: Arbitrary[F[A]]
}

object ArbitraryK {

  def apply[F[_]](implicit F: ArbitraryK[F]): ArbitraryK[F] = F

  implicit def fromApplicative[F[_]: Applicative]: ArbitraryK[F] = new ArbitraryK[F] {
    def toArbitrary[A: Arbitrary]: Arbitrary[F[A]] = Arbitrary(arbitrary[A].map(Applicative[F].pure))
  }
}
