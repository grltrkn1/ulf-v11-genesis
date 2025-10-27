{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ULF.Meta.Performance
-- Description : Performance logging, aggregation and CSI reporting for ULF v11
-- Copyright   : (c) 2025, ULF Genesis Project
-- License     : MIT
--
-- This module implements the "nervous system" for ULF: structures and utilities
-- enabling the system to record raw experimental telemetry, aggregate it into
-- meaningful indices (CSI components), and provide a stable API for the SEI
-- and other modules to query performance.
--
-- The analysis functions here are intentionally conservative and transparent.
-- They provide simple, auditable heuristics suitable for unit tests and early
-- experiments. Replace or extend them with the real `analyze_results.py`
-- pipeline results when integrating the Python analysis artifacts.
module ULF.Meta.Performance
  ( -- * Raw logging types
    PerformanceTick(..)
  , PerformanceLog

    -- * Aggregated report
  , CSIReport(..)

    -- * Accessors
  , getOverallCSI
  , getMetricScore

    -- * Analysis helpers
  , analyzePerformanceLog
  , aggregateLog
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Data.Foldable (foldlM)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Raw logging: PerformanceTick & PerformanceLog
--------------------------------------------------------------------------------

-- | A single recorded observation during an experimental run.
--   'perturbationActive' flags whether the system was under an external
--   perturbation at this cycle (useful for stress-test indexing).
data PerformanceTick = PerformanceTick
  { cycleIndex         :: !Int
  , metricName         :: !String
  , metricValue        :: !Double
  , perturbationActive :: !Bool
  } deriving (Show, Eq, Generic)

instance ToJSON PerformanceTick
instance FromJSON PerformanceTick

-- | The full time-series log for a single run.
--   Conceptually equivalent to a simple time-indexed telemetry table.
type PerformanceLog = [PerformanceTick]

--------------------------------------------------------------------------------
-- Aggregated, human/SEI-facing report: CSIReport
--------------------------------------------------------------------------------

-- | Composite Self-Improvement Index (CSI) report summarizing a single run.
--   The three sub-indices are heuristic and intentionally interpretable:
--     * persistenceIndex      (PI)  -- ability to maintain behavior under perturbation
--     * errorCorrectionIndex  (ECI) -- responsiveness & corrective dynamics
--     * durableLearningIndex  (DLI) -- stable improvement over time
--   'auxiliaryMetrics' stores other named metrics collected/derived.
data CSIReport = CSIReport
  { runId            :: !String
  , condition        :: !String                    -- e.g., "SEI_ON" / "SEI_OFF"
  , persistenceIndex :: !Double                    -- [0..1]
  , errorCorrectionIndex :: !Double                -- [0..1]
  , durableLearningIndex  :: !Double               -- [0..1]
  , overallCSI       :: !Double                    -- composite [0..1]
  , auxiliaryMetrics :: !(Map String Double)       -- other domain-specific metrics
  } deriving (Show, Eq, Generic)

instance ToJSON CSIReport
instance FromJSON CSIReport

--------------------------------------------------------------------------------
-- Simple accessors (stable API for SEI)
--------------------------------------------------------------------------------

-- | Retrieve the overall CSI score.
getOverallCSI :: CSIReport -> Double
getOverallCSI = overallCSI

-- | Retrieve an auxiliary metric by name; default to 1.0 (optimistic) when absent.
--   Defaulting to 1.0 reduces false-positive escalation when optional metrics
--   are missing from older report formats.
getMetricScore :: String -> CSIReport -> Double
getMetricScore metricName report =
  fromMaybe 1.0 (M.lookup metricName (auxiliaryMetrics report))

--------------------------------------------------------------------------------
-- Analysis & aggregation helpers
--------------------------------------------------------------------------------

-- High-level design notes:
-- * These functions implement an auditable, deterministic transformation from
--   PerformanceLog -> CSIReport. That transformation is intentionally simple:
--     - compute per-metric time-series summaries
--     - estimate three interpretable indices from those summaries
--     - combine indices with weighted average to produce overallCSI
-- * This is a prototype analyzer: replace with the more sophisticated Python
--   analysis output when available. However, these heuristics are useful for
--   unit tests and fast local experiments.

-- | Aggregate raw PerformanceLog into a nested map:
--   metricName -> (list of values, countPerturbationTrue)
aggregateLog :: PerformanceLog -> Map String ([Double], Int, Int)
aggregateLog = foldl' ingest M.empty
  where
    ingest :: Map String ([Double], Int, Int) -> PerformanceTick -> Map String ([Double], Int, Int)
    ingest acc (PerformanceTick _ name value pert) =
      let (vals, pertCount, totalCount) = M.findWithDefault ([], 0, 0) name acc
          newVals = value : vals
          newPert = pertCount + (if pert then 1 else 0)
          newTot  = totalCount + 1
      in M.insert name (newVals, newPert, newTot) acc

-- | Analyze a PerformanceLog and produce a CSIReport.
--   'runId' and 'condition' are propagated to the report.
--
--   Heuristic indices:
--     * persistenceIndex: measures how stable a core metric (e.g., "reward")
--       remains when perturbations are present. We compute the ratio of mean
--       under perturbation to overall mean, normalized to [0,1].
--     * errorCorrectionIndex: measures how quickly/strongly the system recovers
--       after drops in a primary metric. Approximated by mean positive delta
--       rate across cycles for the "primary" metric.
--     * durableLearningIndex: measures a monotonic trend: fraction of time the
--       rolling average improved over the run (coarse stability indicator).
--   overallCSI: weighted average of (PI, ECI, DLI).
analyzePerformanceLog :: String -> String -> PerformanceLog -> CSIReport
analyzePerformanceLog runId' condition' logs =
  let
    agg = aggregateLog logs

    -- Primary metric selection heuristic: prefer "reward" or "overall_score" if present
    primaryCandidates = ["reward", "overall_score", "task_success"]
    primaryMetricName = head $ filter (`M.member` agg) primaryCandidates ++ ["reward"] -- default

    -- helper to safe extract numeric aggregates
    mean :: [Double] -> Double
    mean xs = if null xs then 1.0 else sum xs / fromIntegral (length xs)

    getVals :: String -> [Double]
    getVals n = case M.lookup n agg of
                  Just (vs,_,_) -> vs
                  Nothing -> []

    -- Persistence Index (PI): robustness under perturbation for primary metric
    piVal :: Double
    piVal =
      case M.lookup primaryMetricName agg of
        Nothing -> 1.0
        Just (vals, pertCnt, totCnt) ->
          let overallMean = mean vals
              -- extract values recorded during perturbation cycles
              pertVals = [ v | PerformanceTick _ nm v p <- logs, nm == primaryMetricName, p ]
              pertMean = if null pertVals then overallMean else mean pertVals
              ratio = if overallMean <= 0 then 1.0 else (pertMean / overallMean)
          in clamp01 ratio

    -- Error Correction Index (ECI): approximated as mean positive slope following local dips
    -- We compute simple deltas and measure fraction of positive corrections after negative steps.
    eciVal :: Double
    eciVal =
      let vs = reverse (getVals primaryMetricName) -- aggregateLog stored reversed order
          deltas = zipWith (\a b -> b - a) vs (drop 1 vs)
          -- identify indices where a drop occurred (negative delta) and check following delta positive
          pairs = zip deltas (drop 1 deltas)
          corrections = length [ () | (d1,d2) <- pairs, d1 < 0 && d2 > 0 ]
          drops = length [ () | d <- deltas, d < 0 ]
      in if drops == 0 then 1.0 else clamp01 (fromIntegral corrections / fromIntegral drops)

    -- Durable Learning Index (DLI): fraction of time rolling mean increases
    dliVal :: Double
    dliVal =
      let vs = reverse (getVals primaryMetricName)
          window = 5
          rollingMeans xs = [ mean (take window (drop i xs)) | i <- [0 .. max 0 (length xs - window)] ]
          rms = rollingMeans vs
          ups = length [ () | (a,b) <- zip rms (drop 1 rms), b > a ]
          totalWindows = max 1 (length rms - 1)
      in clamp01 (fromIntegral ups / fromIntegral totalWindows)

    -- Auxiliary metrics: compute simple summary means for all logged metrics
    aux :: Map String Double
    aux = M.map (\(vs,_,_) -> mean vs) agg

    -- Combine indices into overall CSI using conservative weights emphasizing stability
    weightPI = 0.35
    weightECI = 0.35
    weightDLI = 0.30
    overall = clamp01 (piVal * weightPI + eciVal * weightECI + dliVal * weightDLI)
  in
    CSIReport
      { runId = runId'
      , condition = condition'
      , persistenceIndex = piVal
      , errorCorrectionIndex = eciVal
      , durableLearningIndex = dliVal
      , overallCSI = overall
      , auxiliaryMetrics = aux
      }

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

-- | Clamp double to [0,1]
clamp01 :: Double -> Double
clamp01 x
  | isNaN x     = 0.0
  | x <= 0.0    = 0.0
  | x >= 1.0    = 1.0
  | otherwise   = x

-- local isNaN fallback (avoid depending on extra modules)
isNaN :: Double -> Bool
isNaN x = x /= x

--------------------------------------------------------------------------------
-- End of ULF.Meta.Performance
--------------------------------------------------------------------------------
