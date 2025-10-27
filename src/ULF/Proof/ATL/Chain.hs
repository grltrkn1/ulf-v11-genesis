{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ULF.Proof.ATL.Chain
-- Description : Chain verifier for ATL timelines — causal & logical coherence checks
-- Copyright   : (c) 2025, ULF Genesis Project
-- License     : MIT
--
-- This module implements the "deductive conscience" for the Alternate Timeline
-- Logic (ATL) planner. Its responsibilities:
--
--  * chainStep        : validate a single inference step (p -> q) under an ATLModel
--  * validateTimelinePath : ensure every consecutive pair in a timeline is valid
--  * failingSteps     : diagnostic helper returning which pairs fail and why
--  * calculatePathProbability : compute a simple success probability for a timeline
--  * filterValidTimelines / findOptimalPath : utilities for planners
--
-- Notes on probability semantics:
--  * For Evt(h, a) we use pWindow m h a (ATL.pWindow) as its success probability (0..1).
--  * For Do(h, a, active) we treat feasibility(m, h) as the gate: if not feasible -> 0.0,
--    otherwise treat the action as probability 1.0 for causal-coherence scoring (the ATL
--    planner may want to include more complex action-success modeling).
--
-- The module intentionally keeps the logic simple and conservative: a timeline must
-- first be logically/causally coherent (validateTimelinePath) before its probability is
-- considered for optimization.

module ULF.Proof.ATL.Chain
  ( -- * Core validators
    chainStep
  , validateTimelinePath
  , failingSteps

    -- * Probability & scoring
  , calculatePathProbability
  , TimelineScore

    -- * Planner helpers
  , filterValidTimelines
  , findOptimalPath
  ) where

import Data.List (zip, tail, maximumBy, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

import ULF.Institutions.ATL
  ( ATLSentence(..)
  , ATLModel(..)
  , holds
  , pWindow
  , Horizon(..)
  )

-- | The atomic chain rule:
--   A transition (p -> q) is valid iff (not (holds m p)) || (holds m q)
--   which is the material-implication check specialized to ATL's semantics.
--
--   In words: a step is invalid only if the premise holds and the conclusion does not.
chainStep :: ATLModel -> ATLSentence -> ATLSentence -> Bool
chainStep m p q = (not (holds m p)) || (holds m q)

-- | Validate an entire timeline: all consecutive pairs must pass 'chainStep'.
--   Returns True if the timeline is logically/causally coherent under model 'm'.
validateTimelinePath :: ATLModel -> [ATLSentence] -> Bool
validateTimelinePath m steps = all (uncurry (chainStep m)) (zip steps (tail steps))

-- | Diagnostics: returns list of failing pairs with their index (i, (step_i, step_{i+1})).
--   The index i corresponds to the first element of the failing pair.
failingSteps :: ATLModel -> [ATLSentence] -> [(Int, (ATLSentence, ATLSentence))]
failingSteps m steps =
  let pairs = zip [0..] (zip steps (tail steps))
  in filter (\(_, (p,q)) -> not (chainStep m p q)) pairs

-- | Type alias for scored timeline: (timeline, probability score)
type TimelineScore = ([ATLSentence], Double)

-- | Calculate a simple path probability for a timeline.
--   Multiplicative model of independent step probabilities:
--     - For (Evt h a): use pWindow m h a
--     - For (Do h a active): if feasible m h then 1.0 else 0.0
--     - For compound sentences: treat as 1.0 when 'holds' true, 0.0 otherwise
--
--   Important: this is a simple, conservative estimator. Callers should only
--   compute probabilities for timelines that pass `validateTimelinePath`.
calculatePathProbability :: ATLModel -> [ATLSentence] -> Double
calculatePathProbability m =
  foldr (\s acc -> stepProb m s * acc) 1.0
  where
    stepProb :: ATLModel -> ATLSentence -> Double
    -- Event probability window
    stepProb mdl (Evt h a) = clamp01 $ pWindow mdl h a
    -- Action feasibility: if feasible -> 1.0 (we assume deterministic for coherence check)
    stepProb mdl (Do h _ active) =
      if feasible mdl h then 1.0 else 0.0
    -- Boolean composites: treat as 1.0 if holds, 0.0 otherwise (conservative)
    stepProb mdl (And p q) = if holds mdl (And p q) then 1.0 else 0.0
    stepProb mdl (Or p q)  = if holds mdl (Or p q)  then 1.0 else 0.0
    stepProb mdl (Imp p q) = if holds mdl (Imp p q) then 1.0 else 0.0
    -- Catch-all: if holds, treat as 1.0, else 0.0
    stepProb mdl s = if holds mdl s then 1.0 else 0.0

-- | Helper: clamp to [0,1] just in case model returns a slightly out-of-range value.
clamp01 :: Double -> Double
clamp01 x
  | x <= 0.0  = 0.0
  | x >= 1.0  = 1.0
  | otherwise = x

-- | Filter a list of candidate timelines, returning only those that are logically valid.
filterValidTimelines :: ATLModel -> [[ATLSentence]] -> [[ATLSentence]]
filterValidTimelines m = filter (validateTimelinePath m)

-- | From a non-empty list of candidate timelines, return the logically valid timeline
--   with the highest calculated path probability, together with its score. If none
--   are valid, returns Nothing.
findOptimalPath :: ATLModel -> [[ATLSentence]] -> Maybe TimelineScore
findOptimalPath m candidates =
  let valid = filterValidTimelines m candidates
      scored :: [TimelineScore]
      scored = map (\t -> (t, calculatePathProbability m t)) valid
  in case scored of
       [] -> Nothing
       xs -> Just $ maximumBy (comparing snd) xs

--------------------------------------------------------------------------------
-- End of ULF.Proof.ATL.Chain
--------------------------------------------------------------------------------
