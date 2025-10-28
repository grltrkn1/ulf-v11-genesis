{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ULF.SEI.Navigator
-- Description : The SEI Navigator — proposes high-impact architectural EvolutionActions
-- Copyright   : (c) 2025, ULF Genesis Project
-- License     : MIT
--
-- The Navigator is the "visionary" half of the SEI: it detects structural failures
-- and proposes high-impact, high-cost EvolutionAction values (GrowToDimension, ConnectFiber, ...).
-- This file provides a conservative, auditable implementation suitable for unit tests
-- and initial integration. Replace the heuristic checks with richer analyses as the
-- rest of the architecture matures.
module ULF.SEI.Navigator
  ( -- * Primary API
    proposeArchitecturalLeap
  , analyzeStructuralFailures
  , determineNextGrowthStep
  , generateGrowthAction

    -- * Types (exported for convenience & testing)
  , EmbodimentJourney(..)
  , ConstructionGoal(..)
  ) where

import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)

-- Use the canonical type for StateDepth defined in ULF.Core.HSG
import ULF.Core.HSG (StateDepth(..))

-- CSIReport & helpers from the Performance subsystem
import ULF.Meta.Performance (CSIReport, getOverallCSI, getMetricScore)

-- EvolutionAction is the language of change
import ULF.Meta.Optimization (EvolutionAction(..))

--------------------------------------------------------------------------------
-- Local (lightweight) EmbodimentJourney representation
-- (Replace with your project's full EmbodimentJourney type when available)
--------------------------------------------------------------------------------

-- | Minimal, testable snapshot of an embodiment journey.
--   The full project may have a richer structure; this small record exists to
--   keep this module self-contained and immediately usable.
data EmbodimentJourney = EmbodimentJourney
  { currentDimensionality :: !StateDepth   -- ^ The agent's current structural depth (NF0..)
  , developmentalHistory  :: [StateDepth]  -- ^ Past achieved dimensional steps
  } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Construction goals the Navigator reasons about (high-level)
--------------------------------------------------------------------------------

data ConstructionGoal
  = AddMotorControl               -- ^ Build NF1
  | ImproveSensoryProcessing      -- ^ Build NF2 capabilities
  | EnhanceMemoryCapacity         -- ^ Build NF3 capabilities
  | ConnectMajorFibers            -- ^ High-level integration (ConnectFiber)
  | MetaStrategyShift String      -- ^ Non-structural: change global reasoning strategy
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Primary API
--------------------------------------------------------------------------------

-- | Top-level navigator entry point.
--   Given a 'CSIReport' and the current 'EmbodimentJourney', propose an
--   architectural EvolutionAction (e.g., GrowToDimension) if a structural fix
--   appears necessary. Returns Nothing when no escalation is advised.
proposeArchitecturalLeap :: CSIReport -> EmbodimentJourney -> Maybe EvolutionAction
proposeArchitecturalLeap report journey =
  case analyzeStructuralFailures report journey of
    Just goal -> Just (generateGrowthAction goal)
    Nothing   -> Nothing

-- | Analyze a CSIReport and journey state to detect structural (hard) failures.
--   Returns a 'ConstructionGoal' if a structural remediation is advisable,
--   otherwise Nothing.
--
--   Heuristics:
--     * If overall CSI is catastrophic (< 0.3) propose the next growth step.
--     * If a critical sensory metric is zero/near-zero and we are shallow, propose sensory improvement.
--     * If persistent memory-failure signals appear at NF2, propose memory enhancement.
analyzeStructuralFailures :: CSIReport -> EmbodimentJourney -> Maybe ConstructionGoal
analyzeStructuralFailures report journey =
  let csi = getOverallCSI report
      curDim = currentDimensionality journey
      sensoryAcc = getMetricScore "SensoryAccuracy" report
      memThroughput = getMetricScore "MemoryThroughput" report
  in if csi < 0.30
       then Just (determineNextGrowthStep curDim)
     else if sensoryAcc <= 0.01 && curDim <= Layer1
       then Just ImproveSensoryProcessing
     else if memThroughput <= 0.05 && curDim <= Layer2
       then Just EnhanceMemoryCapacity
     else Nothing

--------------------------------------------------------------------------------
-- Goal -> Action mapping utilities
--------------------------------------------------------------------------------

-- | Decide the "logical" next growth goal for a given `StateDepth`.
--   This function encodes the canonical developmental roadmap.
determineNextGrowthStep :: StateDepth -> ConstructionGoal
determineNextGrowthStep sd =
  case sd of
    NF0 -> AddMotorControl
    NF1 -> ImproveSensoryProcessing
    NF2 -> EnhanceMemoryCapacity
    NF3 -> MetaStrategyShift "ConsolidateAndOptimize"  -- already high dimension; propose strategy instead

-- | Turn a 'ConstructionGoal' into a concrete serializable 'EvolutionAction'.
generateGrowthAction :: ConstructionGoal -> EvolutionAction
generateGrowthAction goal =
  case goal of
    AddMotorControl             -> GrowToDimension NF1
    ImproveSensoryProcessing    -> GrowToDimension NF2
    EnhanceMemoryCapacity       -> GrowToDimension NF3
    ConnectMajorFibers          -> ConnectFiber "HSG:NF3" "ATL:hazard_3D"
    MetaStrategyShift s         -> ChangeStrategy (parseStrategy s)

-- | A small helper to map textual strategy names into EvolutionStrategy-like tokens.
--   We implement a minimal parser returning ChangeStrategy with an ad-hoc mapping.
--   In the full project you'd import EvolutionStrategy and construct it properly.
parseStrategy :: String -> EvolutionAction
parseStrategy s =
  -- We reuse ChangeStrategy constructor through a placeholder.
  -- If the project's EvolutionStrategy type is imported, replace this logic.
  ChangeStrategy $
    case s of
      "ConsolidateAndOptimize" -> undefined -- replace with actual EvolutionStrategy value
      _                        -> undefined

--------------------------------------------------------------------------------
-- NOTE:
-- The above 'parseStrategy' uses 'ChangeStrategy' constructor which expects
-- an 'EvolutionStrategy' type defined in your Optimization module. Because
-- the precise 'EvolutionStrategy' type may vary, we leave 'parseStrategy'
-- as a placeholder to be completed with your concrete enum/ADT.
--
-- If you prefer to avoid 'undefined' here, replace 'MetaStrategyShift'
-- to generate a safer, textual 'ModifyPrompt' or 'AddAxiom' action instead.
--------------------------------------------------------------------------------

-- End of module
