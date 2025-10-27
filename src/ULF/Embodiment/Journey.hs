{-# LANGUAGE RecordWildCards #-}

-- | ULF.Embodiment.Journey
-- The orchestrator of morphological development and embodied evolution.
-- It manages the progressive transition from NF₀ → NF₃ through guided construction,
-- using the Growth.hs primitives and Navigator decisions.
module ULF.Embodiment.Journey (
    EmbodimentJourney(..),
    DevelopmentalStage(..),
    embodyUntilTarget
) where

import Control.Monad.State
import ULF.Core.HSG (EmbodimentPath(..))
import ULF.Embodiment.Growth (executeGrowthStep)
import ULF.Analytics.Performance (CSIReport, performanceMetrics)
import ULF.SEI.Navigator (getNextGrowthStep)
import ULF.SEI.Types (SEI_Builder(..), StateDepth(..))

-- | Represents a full developmental log and the ongoing embodiment context.
data EmbodimentJourney = EmbodimentJourney {
    startingPoint        :: SEI_Builder,         -- ^ Initial SEI configuration (NF₀)
    currentState          :: SEI_Builder,         -- ^ Current embodiment state
    developmentalHistory  :: [DevelopmentalStage],-- ^ Log of completed stages
    targetArchitecture    :: StateDepth,          -- ^ Desired embodiment target
    path                  :: EmbodimentPath       -- ^ Full planned developmental roadmap
}

-- | Records each achieved developmental milestone.
data DevelopmentalStage = DevelopmentalStage {
    stageNumber        :: Int,
    dimensionalLevel   :: StateDepth,    -- ^ NF₀, NF₁, NF₂, NF₃
    capabilitiesGained :: [String],      -- ^ Qualitative features of this stage
    performanceAtStage :: CSIReport      -- ^ Quantitative performance report
} deriving (Show, Eq)

-- | Drives the embodiment process from current to target state.
embodyUntilTarget :: SEI_Builder -> StateT EmbodimentJourney IO SEI_Builder
embodyUntilTarget builder = do
    journey@EmbodimentJourney{..} <- get
    if currentDimensionality currentState >= targetArchitecture
       then return builder
       else do
           let nextStep = getNextGrowthStep currentState
           newBuilder <- liftIO $ executeGrowthStep currentState nextStep
           let newStage = DevelopmentalStage {
                               stageNumber = length developmentalHistory + 1,
                               dimensionalLevel = currentDimensionality newBuilder,
                               capabilitiesGained = describeCapabilities newBuilder,
                               performanceAtStage = performanceMetrics newBuilder
                           }
           put $ journey {
                    currentState = newBuilder,
                    developmentalHistory = newStage : developmentalHistory
                }
           embodyUntilTarget newBuilder

-- | Placeholder function for extracting qualitative descriptions.
describeCapabilities :: SEI_Builder -> [String]
describeCapabilities _ = ["Structural adaptation", "Refined coordination", "Improved introspection"]
