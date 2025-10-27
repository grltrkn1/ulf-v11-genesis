{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module      : ULF.SEI.Optimizer
-- Description : Tactical Refinement Engine (Optimizer) for ULF v11
-- Copyright   : (c) 2025, ULF Genesis Project
-- License     : MIT
--
-- The Optimizer is the conservative half of the SEI: it proposes low-impact,
-- fast-to-test EvolutionAction values (parameter updates, prompt tweaks, etc.)
-- in response to soft failures reported by the analysis pipeline.
--
-- Exports:
--   * proposeOptimization  -- main entry point used by the SEI control loop
--   * analyzeSoftFailures  -- inspects a CSIReport and finds tunable problems
--   * generateTuningAction -- maps (metric,score) -> EvolutionAction
module ULF.SEI.Optimizer
  ( proposeOptimization
  , analyzeSoftFailures
  , generateTuningAction
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- NOTE: These imports refer to modules already present in the ULF codebase.
-- If you use different names for types/functions, adapt the imports accordingly.
import ULF.Core.Types (Atom)
import ULF.Meta.Performance (CSIReport, getOverallCSI, getMetricScore)
import ULF.Meta.Optimization (EvolutionAction(..))

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

-- | Main entry point for the Optimizer.
-- Given a performance analysis report, returns either:
--   * 'Just EvolutionAction' when a low-impact tuning action is recommended; or
--   * 'Nothing' when no local tuning is appropriate (i.e., either optimal or
--     the problem appears structural and should be escalated to the Navigator).
proposeOptimization :: CSIReport -> Maybe EvolutionAction
proposeOptimization report =
  case analyzeSoftFailures report of
    Just (metric, score) ->
      -- If we detect a tunable issue, produce a suggested EvolutionAction.
      Just (generateTuningAction metric score)
    Nothing ->
      -- Nothing to do locally; escalate to Navigator.
      Nothing

--------------------------------------------------------------------------------
-- Heuristics: analyzing CSI reports for tunable, "soft" failures
--------------------------------------------------------------------------------

-- | Inspect a CSIReport and find the most promising soft failure to tune.
-- Returns a tuple (metricName, metricScore) for the recommended metric to tune.
--
-- Heuristic rules (example):
--  - If overall CSI < catastrophicThreshold -> treat as structural -> Nothing
--  - Otherwise, check a small list of soft, tunable metrics (dialogue, latency)
--  - If any metric is below its soft threshold, return it (lowest-scoring first)
--  - Else, if overall CSI is below a tuning threshold, return ("OverallCSI", csi)
--  - Else, return Nothing
analyzeSoftFailures :: CSIReport -> Maybe (String, Double)
analyzeSoftFailures report =
  let csi = safeGetOverallCSI report
      -- Tunable metrics of interest, in priority order
      metricCandidates = [ ("DialogueEngagement", 0.6)
                         , ("Latency", 0.7)
                         , ("TaskSuccessRate", 0.75)
                         , ("Robustness", 0.7)
                         ]
      -- Collect actual metric scores
      scored = map (\(m,th) -> (m, safeGetMetricScore m report, th)) metricCandidates
      -- find first metric below its threshold (sorted by priority above)
      below = filter (\(_,score,th) -> score < th) scored
  in if csi < catastrophicThreshold
       then Nothing -- catastrophic / structural: escalate to Navigator
       else case below of
              ((m,s,_):_) -> Just (m, s)
              [] ->
                if csi < tuningThreshold
                   then Just ("OverallCSI", csi)
                   else Nothing

-- thresholds (tunable constants)
catastrophicThreshold :: Double
catastrophicThreshold = 0.3

tuningThreshold :: Double
tuningThreshold = 0.8

--------------------------------------------------------------------------------
-- Action generation: mapping (metric,score) -> EvolutionAction
--------------------------------------------------------------------------------

-- | Generate a concrete tuning action for a given metric and score.
-- This is intentionally conservative and human-auditable: prefer small
-- parameter nudges over large, risky changes.
generateTuningAction :: String -> Double -> EvolutionAction
generateTuningAction metric score =
  case metric of
    "DialogueEngagement" ->
      -- Soft prompt modification: small persona and scaffolding change.
      ModifyPrompt
        { targetLLM = "LLM"            -- target LLM instance
        , promptDelta = T.pack "Increase clarity: use shorter sentences and one concrete example per claim."
        , justification = Just $ T.pack $ "DialogueEngagement low (" ++ show score ++ ")"
        }

    "Latency" ->
      -- Reduce planning horizon or fall back to cached policies
      UpdateParameter
        { targetInst = "ATL"
        , parameter  = "planning_horizon"
        , newValue   = clamp (1.0 * suggestedHorizonFactor score) 0.1 2.0
        , justification = Just $ T.pack $ "Latency issue: reduce planning horizon; score=" ++ show score
        }

    "TaskSuccessRate" ->
      -- Slightly increase exploitation (reduce exploration temperature) on LLM
      UpdateParameter
        { targetInst = "LLM"
        , parameter  = "temperature"
        , newValue   = clamp (0.8 * suggestedTemperature score) 0.1 1.5
        , justification = Just $ T.pack $ "TaskSuccessRate less than desired: nudge temperature; score=" ++ show score
        }

    "Robustness" ->
      -- Increase conservative thresholds in ATL (risk_aversion)
      UpdateParameter
        { targetInst = "ATL"
        , parameter  = "risk_aversion"
        , newValue   = clamp (1.0 + (1.0 - score) * 0.5) 0.0 5.0
        , justification = Just $ T.pack $ "Robustness low: increase risk_aversion; score=" ++ show score
        }

    "OverallCSI" ->
      -- Global nudge: reduce SEI learning rate slightly if unstable, or increase if too slow.
      UpdateParameter
        { targetInst = "SEI"
        , parameter  = "learning_rate"
        , newValue   = clamp (if score < 0.5 then 0.001 else 0.01) 1e-6 0.1
        , justification = Just $ T.pack $ "Overall CSI tuning: score=" ++ show score
        }

    -- Fallback: small meta-learning rate tweak
    _ ->
      UpdateParameter
        { targetInst = "SEI"
        , parameter  = "learning_rate"
        , newValue   = 0.005
        , justification = Just $ T.pack $ "Fallback tuning for metric: " ++ metric
        }

--------------------------------------------------------------------------------
-- Small utilities & safe accessors
--------------------------------------------------------------------------------

-- | Safe accessor for overall CSI from CSIReport (wrapper around library accessor).
safeGetOverallCSI :: CSIReport -> Double
safeGetOverallCSI report = fromMaybe 1.0 (getOverallCSIMaybe report)

-- | Safe accessor for specific metric values. Returns 1.0 when metric missing.
safeGetMetricScore :: String -> CSIReport -> Double
safeGetMetricScore metric report = fromMaybe 1.0 (getMetricScoreMaybe metric report)

-- NOTE: The functions `getOverallCSI` and `getMetricScore` used across the
-- codebase might have several possible signatures (pure accessors, IO, Either).
-- To keep the Optimizer module simple and dependency-light we provide wrappers
-- that adapt to two common shapes. If your `ULF.Meta.Performance` module
-- already exposes `getOverallCSI :: CSIReport -> Double` and
-- `getMetricScore :: String -> CSIReport -> Double`, replace these wrappers
-- with direct calls.

-- Try to use library functions if available; otherwise fall back to 'Nothing'.
getOverallCSIMaybe :: CSIReport -> Maybe Double
getOverallCSIMaybe report =
  -- Prefer the library accessor (uncomment if present):
  -- Just (getOverallCSI report)
  -- Fallback: try a generic accessor via record field 'overallCSI' if present.
  case tryLookupOverallCSI report of
    Just v  -> Just v
    Nothing -> Nothing

getMetricScoreMaybe :: String -> CSIReport -> Maybe Double
getMetricScoreMaybe metric report =
  -- Prefer direct accessor if provided:
  -- Just (getMetricScore metric report)
  -- Fallback: try a generic lookup
  tryLookupMetric metric report

-- | Attempt to extract an 'overallCSI' field from an opaque CSIReport via pattern match.
-- Replace / remove this with the true accessor for your CSIReport implementation.
tryLookupOverallCSI :: CSIReport -> Maybe Double
tryLookupOverallCSI _ = Nothing  -- placeholder: implement based on your CSIReport structure

-- | Attempt to lookup arbitrary metric names from CSIReport.
-- Replace / remove with the real accessor; this is a placeholder.
tryLookupMetric :: String -> CSIReport -> Maybe Double
tryLookupMetric _ _ = Nothing

-- | A small helper to keep parameter values within safe bounds.
clamp :: Double -> Double -> Double -> Double
clamp x lo hi = max lo (min hi x)

-- | Suggested horizon scaling factor based on latency score (example heuristic).
suggestedHorizonFactor :: Double -> Double
suggestedHorizonFactor score =
  -- lower score => smaller horizon
  max 0.1 (min 2.0 (1.0 + (score - 0.7)))

-- | Suggested temperature multiplier based on success-rate score (example heuristic).
suggestedTemperature :: Double -> Double
suggestedTemperature score =
  max 0.3 (min 1.5 (1.0 + (0.5 - (score - 0.7))))

--------------------------------------------------------------------------------
-- Notes, integration points and future work
--------------------------------------------------------------------------------
-- 1. Use the MAL (Meta-Axiomatic Ledger) to pick historically-best parameters:
--    Replace heuristics in generateTuningAction with a MAL lookup that finds
--    which parameter historically improved the target metric most reliably.
--
-- 2. Shadow trials: proposed EvolutionAction should create a shadow trial PR
--    (via the existing GitHub Actions workflow) rather than being applied live.
--
-- 3. Safety & validation: before producing UpdateParameter with large changes,
--    the optimizer should simulate the change on a model of the system and
--    verify no invariants are violated.
--
-- 4. Audit logging: every proposed action should be recorded in `sei/decisions/`
--    with justification, expected benefit, and links to the supporting CSI snapshot.
--
-- 5. Bayesian tuning, gradient-based bandits, and meta-gradients: replace the
--    heuristic `generateTuningAction` with a small meta-optimizer that chooses
--    the parameter and magnitude using probabilistic expected improvement.
--
-- End of ULF.SEI.Optimizer
