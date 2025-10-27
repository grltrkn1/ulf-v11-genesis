{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ULF.Institutions.ATL
-- Description : Alternate Timeline Logic (ATL) — Strategic Planning Kernel
-- Copyright   : (c) 2025, ULF Genesis Project
-- License     : MIT
--
-- This module defines the Alternate Timeline Logic (ATL) institution:
-- a hybrid probabilistic-temporal logic used by ULF to plan, evaluate,
-- and optimize action sequences under uncertainty.
--
-- ATL merges modal, temporal, and probabilistic reasoning into a single
-- strategic planning framework. It is the "imagination engine" of ULF.
--
-- Author: ULF Genesis Team

module ULF.Institutions.ATL
  ( -- * Syntax
    Horizon(..)
  , ATLSentence(..)
    -- * Model
  , ATLModel(..)
    -- * Semantics
  , holds
  , pWindow
    -- * Institution wrapper
  , ATLInst(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

import ULF.Core.Types (Atom)
import ULF.Core.Institution (Institution(..))

--------------------------------------------------------------------------------
-- 1. Syntax: Sentences of Alternate Timeline Logic
--------------------------------------------------------------------------------

-- | Temporal horizon classification.
--   * KV – Known, short-term deterministic horizon.
--   * OV – Operational, mid-term tactical horizon.
--   * UV – Ultimate, long-term strategic horizon.
data Horizon = KV | OV | UV
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

-- | A well-formed ATL formula.
--   * `Evt` represents an external event
--   * `Do` represents an intentional action
--   * Boolean connectives mirror CPC structure
data ATLSentence
  = Evt Horizon Atom            -- ^ Expected event: ⟨h,a⟩
  | Do  Horizon Atom Bool       -- ^ Agent action: ⟨h,a,active⟩
  | And ATLSentence ATLSentence -- ^ Conjunction
  | Or  ATLSentence ATLSentence -- ^ Disjunction
  | Imp ATLSentence ATLSentence -- ^ Implication
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- 2. Model: Probabilistic-temporal world representation
--------------------------------------------------------------------------------

-- | ATLModel represents ULF's probabilistic world model at a given time.
data ATLModel = ATLModel
  { renv     :: Atom -> Bool                  -- ^ Current propositional world state
  , hazard   :: Horizon -> Atom -> Double     -- ^ Risk model (hazard rate / likelihood)
  , feasible :: Horizon -> Bool               -- ^ Whether actions are feasible at given horizon
  , trans    :: Horizon -> (Atom -> Bool) -> (Atom -> Bool)
    -- ^ Transition model: causal engine defining how actions/events evolve the world
  }

--------------------------------------------------------------------------------
-- 3. Semantics: Satisfaction relation (⊨)
--------------------------------------------------------------------------------

-- | Compute the effective probability window for an event under a given horizon.
--   This function transforms a raw hazard score into a probability,
--   modulated by the temporal scale.
pWindow :: ATLModel -> Horizon -> Atom -> Double
pWindow m h a =
  let base = hazard m h a
  in case h of
       KV -> min 1.0 (base * 1.0)   -- Known horizon = nearly deterministic
       OV -> min 1.0 (base * 0.8)   -- Operational horizon = slightly uncertain
       UV -> min 1.0 (base * 0.6)   -- Ultimate horizon = high uncertainty

-- | Evaluate whether a sentence 'holds' under a given model.
--   This is the main satisfaction function for ATL.
holds :: ATLModel -> ATLSentence -> Bool
holds m (Evt h a)    = pWindow m h a > 0.5
holds m (Do  h _ _)  = feasible m h
holds m (And p q)    = holds m p && holds m q
holds m (Or  p q)    = holds m p || holds m q
holds m (Imp p q)    = not (holds m p) || holds m q

--------------------------------------------------------------------------------
-- 4. Institution instance
--------------------------------------------------------------------------------

-- | Institution wrapper for Alternate Timeline Logic.
data ATLInst = ATLInst deriving (Eq, Show)

instance Institution ATLInst where
  type Sentence ATLInst = ATLSentence
  type Model    ATLInst = ATLModel
  satisfies = holds

--------------------------------------------------------------------------------
-- 5. Conceptual Commentary
--------------------------------------------------------------------------------
-- The ATL institution defines the *strategic layer* of ULF.
--
-- It bridges epistemic logic, probability, and temporal reasoning.
-- Conceptually, its workflow is:
--
--   1. The SEI (Strategic Executive Intelligence) defines a goal or end state.
--   2. ATL constructs multiple candidate timelines composed of {Evt, Do} pairs.
--   3. Each timeline is evaluated via 'holds' and 'pWindow', producing
--      both truth and expected success probability.
--   4. The Proof.ATL.Chain module validates causal coherence.
--   5. The best path (highest expected probability) is selected for execution.
--
-- *Design Notes:*
--   - ATL differs from FDE and CPC by treating truth probabilistically.
--   - It models *possibility* and *feasibility* rather than binary truth.
--   - The `Horizon` parameter encodes the temporal decay of confidence.
--   - The `hazard` function bridges learned empirical knowledge and
--     symbolic prediction.
--
-- In practice, ATL acts as the "strategic cortex" of ULF:
-- it is capable of risk modeling, timeline forecasting,
-- and self-reflective strategic choice.
--
-- End of ULF.Institutions.ATL
