package structures
package laws

import org.scalacheck.Arbitrary

package object discipline {

  implicit def reduceKindOfArbitraryK[F[_], A](implicit F: ArbitraryK[F], A: Arbitrary[A]): Arbitrary[F[A]] =
   F.toArbitrary[A]
}

