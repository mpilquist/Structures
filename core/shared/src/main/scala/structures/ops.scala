package structures

object ops extends Equal.ToEqualOps
  with Semigroup.ToSemigroupOps
  with Monoid.ToMonoidOps
  with USemigroup.ToUSemigroupOps
  with UMonoid.ToUMonoidOps
  with Exponential.ToExponentialOps
  with Contravariant.ToContravariantOps
  with Functor.ToFunctorOps
  with Apply.ToApplyOps
  with Applicative.ToApplicativeOps
  with Alternative.ToAlternativeOps
  with FlatMap.ToFlatMapOps
  with Monad.ToMonadOps
  with MonadFilter.ToMonadFilterOps
  with MonadAppend.ToMonadAppendOps
  with Foldable.ToFoldableOps
  with Foldable1.ToFoldable1Ops
  with Traverse.ToTraverseOps
  with Traverse1.ToTraverse1Ops
  with Extend.ToExtendOps
  with Extract.ToExtractOps
