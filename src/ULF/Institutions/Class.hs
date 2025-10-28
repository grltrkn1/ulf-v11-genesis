{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | ULF.Institutions.Class
-- | The constitutional typeclass for all logical systems in ULF, based on Institution Theory.
-- | This defines the meta-logical contract that every logic must satisfy to interoperate
-- | within the ULF cognitive architecture (IMU: Intermediate Meta-Universe).
module ULF.Institutions.Class where

-- | An Institution captures the idea that truth is invariant under change of notation.
-- | It abstracts over a category of signatures, sentences over signatures, models for signatures,
-- | and the satisfaction relation between models and sentences, satisfying satisfaction condition.
class Institution i where
  -- | Type of signatures for the institution
  type Sig i :: *
  -- | Type of sentences over a signature
  type Sen i :: *
  -- | Type of models over a signature
  type Mod i :: *

  -- | Satisfaction relation: a model over a signature satisfies a sentence over that signature.
  satisfies :: Mod i -> Sen i -> Bool

  -- | Translation (morphism) of sentences along a signature morphism.
  -- | Given a morphism between signatures, translate a sentence on the target back to the source.
  translateSen :: (Sig i -> Sig i) -> Sen i -> Sen i

  -- | Reduct/translation of models along a signature morphism in the forward direction.
  translateMod :: (Sig i -> Sig i) -> Mod i -> Mod i

  -- | Satisfaction condition (law): translation commutes with satisfaction.
  -- | For any signature morphism σ: Σ -> Σ', model M' over Σ', and sentence φ over Σ',
  -- | we must have: M' ⊨ φ  iff  reduct(σ, M') ⊨ translateSen(σ, φ)
  -- | Note: This is expressed as a law and should hold for all instances.
  satisfactionCondition :: (Sig i -> Sig i) -> Mod i -> Sen i -> Bool
  satisfactionCondition sigma m' phi =
    satisfies m' phi == satisfies (translateMod sigma m') (translateSen sigma phi)
