{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ULF.Institutions.FDE
-- Description : First-Degree Entailment (FDE) — a four-valued paraconsistent logic
-- Copyright   : (c) 2025, ULF Genesis Project
-- License     : MIT
--
-- This module implements a conservative First-Degree Entailment (FDE) system:
--  - Four truth values: N (neither), F (false), T (true), B (both)
--  - Syntax identical to CPC (propositional connectives)
--  - Semantics: operations defined by the four-valued lattice
--  - Institution wrapper for integration with ULF's institutions framework
module ULF.Institutions.FDE
  ( -- * Syntax & Values
    FDE4(..)
  , FDESentence(..)
    -- * Model & Evaluation
  , FDEModel
  , satisfiesFDE
  , evalFDE
    -- * Institution wrapper
  , FDEInst(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import ULF.Core.Types (Atom)
import ULF.Core.Institution (Institution(..))

--------------------------------------------------------------------------------
-- 1. Truth values: the four-valued lattice
--------------------------------------------------------------------------------

-- | The four truth values of First-Degree Entailment (FDE).
--   * N = Neither true nor false (gap)
--   * F = False only
--   * T = True only
--   * B = Both true and false (glut / contradiction)
data FDE4 = N | F | T | B
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

-- We define a lattice ordering for FDE4 that matches the usual presentation.
-- For operations we will treat conjunction as infimum (meet) and disjunction
-- as supremum (join) w.r.t. a chosen lattice ordering.
--
-- A common ordering (information ordering) is:
--    F  <  N  <  T
--    F  <  B  <  T
-- where N and B are incomparable; however for implementing min/max we use a
-- custom lattice defined below.

-- | A helper giving a lattice-index for comparison when computing meet/join.
-- We provide a partial order implementation via explicit tables for meet/join.
fdeMeet :: FDE4 -> FDE4 -> FDE4
fdeMeet F F = F
fdeMeet F N = F
fdeMeet N F = F
fdeMeet F B = F
fdeMeet B F = F
fdeMeet F T = F
fdeMeet T F = F

fdeMeet N N = N
fdeMeet N B = F   -- N and B => conflict resolves to F as conservative choice
fdeMeet B N = F
fdeMeet N T = N
fdeMeet T N = N

fdeMeet B B = B
fdeMeet B T = B
fdeMeet T B = B
fdeMeet T T = T

-- | Disjunction as join on the lattice
fdeJoin :: FDE4 -> FDE4 -> FDE4
fdeJoin = \a b -> case (a,b) of
  (T, _) -> T
  (_, T) -> T
  (B, _) -> B
  (_, B) -> B
  (N, _) -> N
  (_, N) -> N
  (F, F) -> F

-- Note: The meet/join tables above are conservative and chosen to reflect a
-- practical handling of contradictions and gaps. If you prefer a different
-- lattice ordering (e.g., bilattice structures), replace fdeMeet/fdeJoin
-- accordingly.

-- | Negation on FDE4: flips purely true/false while leaving N/B as they are.
-- Intuition:
--   T -> F
--   F -> T
--   N -> N
--   B -> B
fdeNeg :: FDE4 -> FDE4
fdeNeg T = F
fdeNeg F = T
fdeNeg N = N
fdeNeg B = B

--------------------------------------------------------------------------------
-- 2. Syntax: FDE sentences (same shape as CPC)
--------------------------------------------------------------------------------

-- | Propositional syntax for FDE. Parallels CPC's syntax.
data FDESentence
  = FDEVar Atom
  | FDENot FDESentence
  | FDEAnd FDESentence FDESentence
  | FDEOr  FDESentence FDESentence
  | FDEImp FDESentence FDESentence  -- implication as derived: ¬p ∨ q
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- 3. Models and evaluation
--------------------------------------------------------------------------------

-- | A model for FDE maps each atomic proposition (Atom) to one of the four
--   truth values.
type FDEModel = Atom -> FDE4

-- | Evaluate a sentence under a model, returning an FDE4 truth value.
--   This is the core satisfaction function for FDE.
satisfiesFDE :: FDEModel -> FDESentence -> FDE4
satisfiesFDE m (FDEVar a)         = m a
satisfiesFDE m (FDENot φ)         = fdeNeg (satisfiesFDE m φ)
satisfiesFDE m (FDEAnd φ ψ)       = fdeMeet (satisfiesFDE m φ) (satisfiesFDE m ψ)
satisfiesFDE m (FDEOr  φ ψ)       = fdeJoin (satisfiesFDE m φ) (satisfiesFDE m ψ)
-- implication defined as ¬φ ∨ ψ (standard material implication form)
satisfiesFDE m (FDEImp φ ψ)       =
  let notφ = fdeNeg (satisfiesFDE m φ)
      val  = fdeJoin notφ (satisfiesFDE m ψ)
  in val

-- | Alias for readability in some contexts.
evalFDE :: FDEModel -> FDESentence -> FDE4
evalFDE = satisfiesFDE

--------------------------------------------------------------------------------
-- 4. Institution wrapper
--------------------------------------------------------------------------------

-- | Thin wrapper type for creating an Institution instance for FDE.
data FDEInst = FDEInst deriving (Eq, Show)

instance Institution FDEInst where
  type Sentence FDEInst = FDESentence
  type Model    FDEInst = FDEModel
  -- In the institutions framework, 'satisfies' often has type Model -> Sentence -> Bool
  -- For FDE we expose the multi-valued result via 'evalFDE'. To keep the same
  -- type signature as other institutions we provide a boolean predicate
  -- convenience: a sentence is "designated true" iff its value is T or B
  --
  -- However, the Institution typeclass used across ULF may expect a Bool-returning
  -- satisfies; if so, this instance adapts by considering designated truth.
  satisfies m s = let v = evalFDE m s in isDesignated v

-- | Predicate: which FDE values are "designated" as acceptable truth for entailment.
-- A common choice is {T, B} (both true-only and both true-and-false are designated).
isDesignated :: FDE4 -> Bool
isDesignated T = True
isDesignated B = True
isDesignated _ = False

--------------------------------------------------------------------------------
-- 5. Notes & integration guidance
--------------------------------------------------------------------------------
-- * This implementation returns an FDE4 value for full-information consumers
--   (use 'evalFDE'), while the Institution 'satisfies' method yields a boolean
--   via 'isDesignated' to fit classical-style entailment checks.
-- * For richer workflows, consider exposing both functions to callers:
--     - 'evalFDE' for multi-valued reasoning and diagnostics (preferred by SEI)
--     - 'satisfies' for quick designated-true checks
-- * If you want algebraic bilattice behavior (Belnap logic) or a different
--   designated set, adjust fdeMeet/fdeJoin, fdeNeg, and 'isDesignated'.
-- * To implement an InstitutionMorphism from FDE -> CPC or FDE -> Probabilistic
--   modules, decide how to collapse the four values into two (e.g., map T -> True,
--   F -> False, B -> Unknown/Conflict, N -> Unknown), or map into probability priors.
--
-- End of FDE module
