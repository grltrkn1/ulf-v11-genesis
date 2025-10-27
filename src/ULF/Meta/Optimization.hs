{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ULF.Meta.Optimization
-- Description : The Language of Self-Modification and the Ultimate Objective Function (UOF)
-- Copyright   : (c) 2025, ULF Genesis Project
-- License     : MIT
--
-- This module defines the "will" of the ULF system — the formal grammar
-- and evaluative logic that allow the SEI (Self-Evolution Institution)
-- to express, assess, and justify its own self-modifications.
--
-- Together with 'ULF.Meta.Performance', it completes the feedback loop:
--   Observation → Evaluation → Intention → Transformation.
--
-- It provides:
--
--   * 'EvolutionAction' — a typed vocabulary of self-modifications
--   * 'UOF' (Ultimate Objective Function) — a scalar utility assigning
--     goodness to potential changes given empirical evidence.
--
--   The result: the SEI can not only learn, but *decide what to become*.
module ULF.Meta.Optimization
  ( -- * Evolutionary language
    EvolutionAction(..)
  , EvolutionStrategy(..)
  , StateDepth(..)
  , Sentence

    -- * UOF configuration and evaluation
  , UOFConfig(..)
  , calculateUOF

    -- * Internal helpers (optional export)
  , calculateComplexityPenalty
  , calculateRiskPenalty
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

-- Import the performance metrics (for CSIReport, getMetricScore, etc.)
import ULF.Meta.Performance (CSIReport(..), getMetricScore)

--------------------------------------------------------------------------------
-- Fundamental supporting types
--------------------------------------------------------------------------------

-- | Abstract representation of state depth or complexity tier.
--   Think of this as a proxy for architectural depth (e.g. number of cognitive
--   layers or feedback tiers currently instantiated).
data StateDepth
  = Layer1     -- ^ purely reactive / sensorimotor
  | Layer2     -- ^ reflective / local adaptation
  | Layer3     -- ^ meta-cognitive
  | Layer4     -- ^ dialectical / structural synthesis
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON StateDepth
instance FromJSON StateDepth

-- | Strategies for macro-level optimization of the SEI itself.
--   These represent shifts in search or reasoning methodology.
data EvolutionStrategy
  = GradientAscent
  | BayesianSearch
  | EvolutionarySampling
  | DialecticalSynthesis
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON EvolutionStrategy
instance FromJSON EvolutionStrategy

-- | Abstract linguistic unit for adding axioms to knowledge modules.
--   Represented minimally as text, but in the full architecture this
--   would be a logical or semantic object.
type Sentence = Text

--------------------------------------------------------------------------------
-- EvolutionAction: the Language of Change
--------------------------------------------------------------------------------

-- | Represents a declarative, verifiable act of self-modification.
--
--   Each constructor is an "utterance" in the language of becoming.
--   These values are serializable and thus auditable — a complete history
--   of evolution can be stored as a simple list of 'EvolutionAction's.
data EvolutionAction
    -- | Fine-grained parameter update (tactical)
  = UpdateParameter
      { targetInst :: String   -- ^ subsystem or module name
      , parameter  :: String   -- ^ parameter name
      , newValue   :: Double   -- ^ numeric target value
      }
    -- | Modification of an LLM or agent prompt
  | ModifyPrompt
      { targetLLM   :: String
      , promptDelta :: Text
      }
    -- | Architectural growth — adding depth or scope
  | GrowToDimension StateDepth
    -- | Connecting two subsystems with a new fiber (bridge)
  | ConnectFiber String String
    -- | Strategic shift in optimization approach
  | ChangeStrategy EvolutionStrategy
    -- | Addition of an axiom or declarative belief
  | AddAxiom
      { targetInst :: String
      , newAxiom   :: Sentence
      }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON EvolutionAction
instance FromJSON EvolutionAction

--------------------------------------------------------------------------------
-- The Ultimate Objective Function (UOF)
--------------------------------------------------------------------------------

-- | Configuration of UOF weightings and aversions.
--
--   Conceptually, this defines the SEI's *moral geometry*:
--   what it values (w_csi, w_dli) and what it fears (λ penalties).
data UOFConfig = UOFConfig
  { w_csi    :: Double  -- ^ weight for stability (overall CSI)
  , w_dli    :: Double  -- ^ weight for durable learning
  , lambda_c :: Double  -- ^ aversion to complexity
  , lambda_r :: Double  -- ^ aversion to risk
  , lambda_k :: Double  -- ^ aversion to computational cost
  } deriving (Eq, Show, Generic)

instance ToJSON UOFConfig
instance FromJSON UOFConfig

--------------------------------------------------------------------------------
-- UOF evaluation
--------------------------------------------------------------------------------

-- | The Ultimate Objective Function.
--
--   Given a configuration (moral stance), a performance report (empirical data),
--   and a candidate action (intent), returns a scalar utility.
--
--   The SEI seeks to *maximize* this value over possible 'EvolutionAction's.
calculateUOF :: UOFConfig -> CSIReport -> EvolutionAction -> Double
calculateUOF config report action =
  let
    -- === 1. Gains (benefits) ===
    gain =
      (w_csi config * overallCSI report)
        + (w_dli config * durableLearningIndex report)

    -- === 2. Penalties (costs) ===
    complexityPenalty = calculateComplexityPenalty action
    riskPenalty       = calculateRiskPenalty report
    costPenalty       = getMetricScore "Latency" report  -- proxy for computational cost

    -- === 3. Combine into net utility ===
    total =
      gain
        - (lambda_c config * complexityPenalty)
        - (lambda_r config * riskPenalty)
        - (lambda_k config * costPenalty)
  in
    total

--------------------------------------------------------------------------------
-- Penalty models (heuristic, pluggable)
--------------------------------------------------------------------------------

-- | A heuristic measure of how complex an action is.
--   Complexity is proportional to the structural magnitude of change.
calculateComplexityPenalty :: EvolutionAction -> Double
calculateComplexityPenalty action =
  case action of
    UpdateParameter{} -> 0.1
    ModifyPrompt{}    -> 0.2
    GrowToDimension d -> 0.4 * fromIntegral (fromEnum d + 1)
    ConnectFiber _ _  -> 0.5
    ChangeStrategy{}  -> 0.6
    AddAxiom{}        -> 0.3

-- | A coarse proxy for systemic risk, estimated from empirical volatility.
--   Here we approximate risk as the inverse of persistence.
--   When persistence is low, the system is behaving unstably → higher risk.
calculateRiskPenalty :: CSIReport -> Double
calculateRiskPenalty report =
  let p = persistenceIndex report
  in  1.0 - min 1.0 (max 0.0 p)

--------------------------------------------------------------------------------
-- End of ULF.Meta.Optimization
--------------------------------------------------------------------------------
