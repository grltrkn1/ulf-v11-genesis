{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ULF.Institutions.K
-- Description : Modal Logic K (Kripke semantics) — The Engine of Possibility
-- Copyright   : (c) 2025, ULF Genesis Project
-- License     : MIT
--
-- This module implements the core of normal modal logic K using Kripke-style
-- possible-world semantics. It provides:
--   * a compact AST for modal formulas (Box as primitive),
--   * a parametric Kripke model type 'KModel w' (worlds of type 'w'),
--   * the satisfaction relation evaluated at a specific world,
--   * an 'Institution' instance wrapper for use in the ULF architecture.
--
-- Note: The module is deliberately parametric in the world type 'w' so that
-- different consumers (ATL, epistemic extensions, planning modules) may use
-- whatever world representation is most appropriate (Int, URI, record, etc.).
module ULF.Institutions.K
  ( -- * Syntax
    KSentence(..)
  , mkDiamond

    -- * Semantics / Models
  , KModel(..)
  , satisfiesK
  , evalKAt

    -- * Institution wrapper
  , KInst(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import ULF.Core.Types (Atom)
import ULF.Core.Institution (Institution(..))

--------------------------------------------------------------------------------
-- 1. Syntax: modal formulas (Box primitive)
--------------------------------------------------------------------------------

-- | Well-formed formulas of (normal) modal logic K.
-- We include only a minimal set of connectives; others can be defined as
-- derived forms (e.g., Diamond).
data KSentence
  = KVar  Atom              -- ^ atomic proposition
  | KNot  KSentence         -- ^ negation ¬φ
  | KAnd  KSentence KSentence -- ^ conjunction φ ∧ ψ
  | KOr   KSentence KSentence -- ^ disjunction φ ∨ ψ
  | KImp  KSentence KSentence -- ^ implication φ → ψ
  | Box   KSentence         -- ^ necessity □φ (primitive modal operator)
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | Derived constructor: ◇φ ≡ ¬□¬φ
mkDiamond :: KSentence -> KSentence
mkDiamond phi = KNot (Box (KNot phi))

--------------------------------------------------------------------------------
-- 2. Semantics: Kripke models (parametric in world type 'w')
--------------------------------------------------------------------------------

-- | A Kripke model parameterized by a world type 'w'.
--   * 'universe' enumerates the worlds (optionally)
--   * 'succs' is the accessibility relation R: for each world, its accessible worlds
--   * 'val' maps an Atom to the set of worlds in which it is true
--
-- Using a list for universe is convenient for small finite models; consumers
-- may use other representations as appropriate.
data KModel w = KModel
  { universe :: [w]           -- ^ the set of worlds W (optional enumeration)
  , succs    :: w -> [w]      -- ^ accessibility relation R(w) -> [w]
  , val      :: Atom -> [w]   -- ^ valuation V: which worlds make an atom true
  }

--------------------------------------------------------------------------------
-- 3. Satisfaction relation: evaluated at a particular world
--------------------------------------------------------------------------------

-- | Evaluate whether a KSentence is satisfied in model 'm' at world 'w'.
--   Requires Eq w so we can compare worlds for membership in valuation/accessibility lists.
satisfiesK :: (Eq w) => KModel w -> w -> KSentence -> Bool
satisfiesK m w = go
  where
    -- helper: atom true in world?
    atomTrue a = w `elem` val m a

    go :: KSentence -> Bool
    go (KVar a)     = atomTrue a
    go (KNot φ)     = not (go φ)
    go (KAnd φ ψ)   = go φ && go ψ
    go (KOr  φ ψ)   = go φ || go ψ
    go (KImp φ ψ)   = (not (go φ)) || go ψ
    go (Box φ)      =
      let accessible = succs m w
      in all (\u -> satisfiesK m u φ) accessible

-- | Helper alias: identical to 'satisfiesK' but named for readability in some contexts.
evalKAt :: (Eq w) => KModel w -> w -> KSentence -> Bool
evalKAt = satisfiesK

--------------------------------------------------------------------------------
-- 4. Institution wrapper: KInst w
--------------------------------------------------------------------------------

-- | A thin wrapper type to allow creating an 'Institution' instance that is
-- parametric in the world type 'w'.
data KInst w = KInst deriving (Eq, Show)

-- We provide an Institution instance for any 'KInst w' as long as world equality
-- is available. This associates:
--   * Sentence (KInst w) = KSentence
--   * Model    (KInst w) = KModel w
-- and uses 'satisfiesK' as the satisfaction relation.
instance (Eq w) => Institution (KInst w) where
  type Sentence (KInst w) = KSentence
  type Model    (KInst w) = KModel w
  satisfies = satisfiesK

--------------------------------------------------------------------------------
-- 5. Notes and recommended extensions
--------------------------------------------------------------------------------
-- * The current Box semantics implements plain system K. To encode stronger
--   modal systems, impose constraints on the accessibility relation:
--     - T (reflexive):     add reflexivity => axiom □φ → φ (knowledge)
--     - 4 (transitive):    add transitivity => axiom □φ → □□φ
--     - B (symmetric):     add symmetry
--   Those constraints are enforced by using a model construction that guarantees
--   the corresponding relational properties.
--
-- * For ATL/temporal variants, replace 'succs' with a transition function that
--   returns *successor states* (possibly with labels / actions). The 'Box'
--   operator maps naturally to temporal "always" modalities (e.g., AG).
--
-- * Epistemic logics typically use an indexed accessibility relation (one per agent):
--     succs :: Agent -> w -> [w]
--   and the syntax includes agent-indexed modalities (□_a).
--
-- * For performance in large models prefer Set-based valuations / adjacency
--   structures (Data.Set / Data.Map) instead of lists; this file intentionally
--   uses lists for clarity and small-model convenience.
--------------------------------------------------------------------------------
