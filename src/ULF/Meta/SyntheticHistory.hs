{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ULF.Meta.SyntheticHistory
-- Description : Counterfactual simulation and synthetic history generation for ULF v11
-- Copyright   : (c) 2025, ULF Genesis Project
-- License     : MIT
--
-- This module implements a practical, auditable prototype of the "Imagination Engine".
-- It provides functions to:
--   * reconstruct a system state before an action (best-effort / heuristic),
--   * apply a counterfactual action to that state,
--   * run a simple probabilistic simulation forward to produce a PerformanceLog.
--
-- The design is intentionally modular: replace the simple stochastic engine here
-- with your real ATLModel-driven simulator or a learned generative model (diffusion
-- model) when available.
module ULF.Meta.SyntheticHistory
  ( -- * Types
    SystemState(..)
  , SimulationConfig(..)

    -- * Main entry points
  , generateSyntheticHistory
  , runSimulation
  , reconstructStateBeforeAction
  , applyAction

    -- * Utilities (exposed for testing)
  , defaultSimConfig
  ) where

import GHC.Generics (Generic)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Data.Text (Text, pack)
import qualified Data.Map.Strict as M

-- Import the PerformanceLog / PerformanceTick types
import ULF.Meta.Performance (PerformanceLog, PerformanceTick(..))
import ULF.Meta.Optimization (EvolutionAction(..))

-- NOTE:
-- In the full ULF system, the ATLModel (and other domain models) should be imported
-- and used to drive the simulator. This prototype uses a lightweight 'SystemState'
-- placeholder so the functions are compilable and immediately usable in tests.

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A small, serializable snapshot of the system used for simulation.
--   In the full system this would be a rich structure (symbolic state, numeric
--   parameters, instantiated ATLModel, etc.). Keep it small here to allow
--   prototyping while the richer model is developed.
data SystemState = SystemState
  { ssPrimaryValue :: Double            -- ^ the primary metric (e.g., "reward" or "score")
  , ssAux          :: M.Map String Double -- ^ other named internal vars
  } deriving (Show, Eq, Generic)

-- | Runtime configuration for the lightweight simulator.
data SimulationConfig = SimulationConfig
  { simNoiseStd      :: Double  -- ^ standard deviation of Gaussian-like noise applied each cycle
  , simPerturbProb   :: Double  -- ^ probability an external perturbation occurs in a cycle
  , simPerturbImpact :: Double  -- ^ magnitude of perturbation effect on primary metric
  , simRecoveryRate  :: Double  -- ^ fraction of lost value recovered per cycle (if perturbed)
  , simPrimaryName   :: String  -- ^ metric name to be used in PerformanceTick entries
  } deriving (Show, Eq, Generic)

-- | Conservative default configuration useful for unit-tests and early experiments.
defaultSimConfig :: SimulationConfig
defaultSimConfig = SimulationConfig
  { simNoiseStd = 0.01
  , simPerturbProb = 0.05
  , simPerturbImpact = 0.2
  , simRecoveryRate = 0.1
  , simPrimaryName = "reward"
  }

--------------------------------------------------------------------------------
-- High-level API
--------------------------------------------------------------------------------

-- | Generate a synthetic PerformanceLog by:
--   1. Reconstructing a SystemState prior to 'counterfactualAction' (heuristic)
--   2. Applying the counterfactual action
--   3. Running the probabilistic simulator for 'numCycles' cycles
--
--   * 'historyActions' is the list of past EvolutionAction values (the real actions taken).
--   * This function returns IO because it uses randomness for stochastic simulation.
generateSyntheticHistory
  :: SimulationConfig
  -> [EvolutionAction]   -- ^ real historical actions (used to reconstruct state heuristically)
  -> EvolutionAction     -- ^ the counterfactual action to apply instead
  -> Int                 -- ^ number of cycles to simulate forward
  -> IO PerformanceLog
generateSyntheticHistory simCfg historyActions counterfactualAction numCycles = do
  -- 1) reconstruct an initial state heuristically
  let preState = reconstructStateBeforeAction historyActions

  -- 2) apply the counterfactual
  let cfState = applyAction preState counterfactualAction

  -- 3) run the simulation forward for numCycles
  runSimulation simCfg cfState numCycles

--------------------------------------------------------------------------------
-- State reconstruction and action application (heuristic prototypes)
--------------------------------------------------------------------------------

-- | Heuristic reconstruction of the state before the last action in the history.
--   This is intentionally conservative: it uses simple inverse heuristics so it is
--   auditable and deterministic given the same inputs.
--
--   Replace this with a domain-specific approach (e.g., replaying logged state
--   checkpoints or using ATLModel's deterministic reducer) when available.
reconstructStateBeforeAction :: [EvolutionAction] -> SystemState
reconstructStateBeforeAction actions =
  -- Start from neutral baseline and "undo" simple UpdateParameter / Grow actions
  let baseline = SystemState { ssPrimaryValue = 1.0, ssAux = M.empty }
  in foldr undo simpleBaseline actions
  where
    simpleBaseline = baseline
    undo :: EvolutionAction -> SystemState -> SystemState
    undo act st =
      case act of
        -- If the real history updated a parameter that likely increased reward,
        -- conservatively assume prior value was slightly lower.
        UpdateParameter { parameter = p, newValue = v } ->
          if p == "reward_scale"
            then st { ssPrimaryValue = ssPrimaryValue st / (1.0 + 0.05 * v) }
            else st
        -- If they grew a dimension, the prior state was simpler (slightly lower capability)
        GrowToDimension d ->
          st { ssPrimaryValue = ssPrimaryValue st * (1.0 - 0.05 * fromIntegral (fromEnum d)) }
        -- Other actions: make conservative, small adjustments
        ModifyPrompt{} -> st { ssPrimaryValue = ssPrimaryValue st * 0.99 }
        ChangeStrategy{} -> st { ssPrimaryValue = ssPrimaryValue st * 0.98 }
        ConnectFiber{} -> st { ssPrimaryValue = ssPrimaryValue st * 0.995 }
        AddAxiom{} -> st { ssPrimaryValue = ssPrimaryValue st * 1.0 }

--------------------------------------------------------------------------------

-- | Apply an EvolutionAction to a SystemState to produce a counterfactual state.
--   This is a simple, auditable transformation. When a full model is available,
--   replace with a semantic application (e.g., update ATLModel and recompute derived features).
applyAction :: SystemState -> EvolutionAction -> SystemState
applyAction st act =
  case act of
    UpdateParameter { parameter = p, newValue = v } ->
      if p == "reward_scale"
        then st { ssPrimaryValue = ssPrimaryValue st * (1.0 + 0.01 * v) }
        else st { ssAux = M.insert p v (ssAux st) }

    ModifyPrompt { promptDelta = d } ->
      -- Treat prompt changes as small behavioral nudges
      st { ssPrimaryValue = ssPrimaryValue st * 1.005 }

    GrowToDimension d ->
      -- Structural growth produces larger capability jumps
      st { ssPrimaryValue = ssPrimaryValue st * (1.0 + 0.05 * fromIntegral (fromEnum d + 1)) }

    ConnectFiber a b ->
      -- Connecting modules yields small improvements (domain-dependent)
      st { ssPrimaryValue = ssPrimaryValue st * 1.02 }

    ChangeStrategy s ->
      -- Strategy changes are risky; apply conservative effect
      case s of
        -- treat certain strategies as exploration (slightly reduce immediate primary metric)
        _ -> st { ssPrimaryValue = ssPrimaryValue st * 0.98 }

    AddAxiom { newAxiom = _ } ->
      -- Adding beliefs may or may not change primary metric; leave unchanged
      st

--------------------------------------------------------------------------------
-- Simulation engine
--------------------------------------------------------------------------------

-- | Run a simple stochastic simulation producing a PerformanceLog.
--   Each cycle produces a PerformanceTick for the configured primary metric.
--   The engine:
--     * applies small gaussian-like noise,
--     * with some probability injects a perturbation event (larger drop),
--     * when perturbed, recovery occurs gradually according to simRecoveryRate.
--
--   This is intentionally simple so that results are reproducible and explainable.
runSimulation :: SimulationConfig -> SystemState -> Int -> IO PerformanceLog
runSimulation simCfg initState cycles =
  simLoop 0 initState False []
  where
    name = simPrimaryName simCfg

    -- Sim loop: cycle index, current state, whether currently perturbed, accumulated log
    simLoop :: Int -> SystemState -> Bool -> PerformanceLog -> IO PerformanceLog
    simLoop idx st isPert acc
      | idx >= cycles = return (reverse acc)
      | otherwise = do
          -- noise ~ Uniform(-noiseStd, +noiseStd) as a simple stand-in for gaussian
          noise <- randomRIO (-(simNoiseStd simCfg), simNoiseStd simCfg)

          -- decide whether a fresh perturbation occurs this cycle
          pdraw <- randomRIO (0.0, 1.0)
          let newPert = if pdraw < simPerturbProb simCfg then True else isPert

          -- apply effects
          let base = ssPrimaryValue st + noise
              afterPert =
                if newPert && not isPert
                   then base - simPerturbImpact simCfg  -- immediate drop when perturbation begins
                   else if newPert && isPert
                     then base * (1.0 - simPerturbImpact simCfg * 0.1) -- ongoing penalty
                     else base

              -- if recovering, gradually move primary value toward pre-perturb baseline
              recovered =
                if not newPert && isPert
                  then afterPert + simRecoveryRate simCfg * (ssPrimaryValue initState - afterPert)
                  else afterPert

              -- clamp to some reasonable numeric bounds
              clamped = max 0.0 recovered

              tick = PerformanceTick { cycleIndex = idx
                                     , metricName = name
                                     , metricValue = clamped
                                     , perturbationActive = newPert
                                     }

              nextState = st { ssPrimaryValue = clamped } -- internal state follows metric for simplicity

          simLoop (idx + 1) nextState newPert (tick : acc)

--------------------------------------------------------------------------------
-- End of module
--------------------------------------------------------------------------------
