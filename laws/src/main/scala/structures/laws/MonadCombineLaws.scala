package structures
package laws

trait MonadCombineLaws[F[_]] extends MonadFilterLaws[F] with AlternativeLaws[F] {

  implicit val typeClass: MonadCombine[F]

  import MonadCombine.ops._

  // TODO
}

object MonadCombineLaws {
  def apply[F[_]: MonadCombine]: MonadCombineLaws[F] = new MonadCombineLaws[F] {
    val typeClass = MonadCombine[F]
  }
}
