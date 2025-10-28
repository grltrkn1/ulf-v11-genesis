{-# LANGUAGE OverloadedStrings #-}

-- | ================================================================
-- |  ULF v11 Genesis Project — Main.hs
-- |  ---------------------------------------------------------------
-- |  The Heartbeat and Central Nervous System of the Autonomous Mind.
-- |  This module orchestrates the full Autonomous Scientific Cycle
-- |  (ASC), coordinating all subsystems of ULF: SEI, HSG, ATL, and UOF.
-- |
-- |  It embodies the principle:
-- |    "Cognition is the orchestration of process across time."
-- |
-- |  Author: ULF Genesis Core Team
-- |  License: MIT
-- | ================================================================

module Main where

import System.Environment (getArgs)
import Control.Monad (when)
import ULF.Config (Config, loadConfig)
import qualified ULF.SEI.Optimizer as Optimizer
import qualified ULF.SEI.Navigator as Navigator
import ULF.Meta.Performance (CSIReport, runAnalysisPipeline)
import ULF.Meta.Optimization (EvolutionAction)
import ULF.Meta.UOF (calculateUOF)
import ULF.Core.HSG (EmbodimentJourney)
import ULF.SEI (initializeSEI, journeyState)
import ULF.System.Feedback (triggerSEIFeedbackAgent)
import ULF.System.Execution (runExperimentPipeline, runShadowTrial)
import ULF.Utils.Pretty (printHeader)

-- ================================================================
-- |  MAIN ENTRY POINT
-- ================================================================

-- | The system's entry point: determines mode of operation.
--   Equivalent to the "Director" of the cognitive theater.
main :: IO ()
main = do
  printHeader "ULF v11 Genesis — Autonomous Scientific Engine"
  args <- getArgs
  config <- loadConfig "config/experiment.yaml"

  case args of
    ["--run-cycle"] -> do
      putStrLn ">> Initiating Autonomous Scientific Cycle..."
      runAutonomousCycle config

    ["--run-benchmark"] -> do
      putStrLn ">> Running CHC-AI Benchmark Suite..."
      runFullBenchmark config

    ["--validate-pr", prNumber] -> do
      putStrLn $ ">> Validating Pull Request #" ++ prNumber
      runPRValidation config prNumber

    _ -> printUsage


-- ================================================================
-- |  AUTONOMOUS SCIENTIFIC CYCLE (ASC)
-- ================================================================

-- | Executes one full iteration of the four-stage ASC.
--   Each iteration is one "heartbeat" of the ULF system.
runAutonomousCycle :: Config -> IO ()
runAutonomousCycle config = do
  putStrLn "\n[Stage 0] Initializing SEI and Environment..."
  seiBuilder <- initializeSEI config

  ---------------------------------------------------------------
  -- Stage 1: Experience — "Run the Experiment"
  ---------------------------------------------------------------
  putStrLn "\n[Stage 1] Running Experiment Pipeline..."
  rawLogPath <- runExperimentPipeline config

  ---------------------------------------------------------------
  -- Stage 2 & 3: Reflection + Insight — "Analyze and Understand"
  ---------------------------------------------------------------
  putStrLn "\n[Stage 2/3] Analyzing Experiment Results..."
  csiReport <- runAnalysisPipeline rawLogPath

  ---------------------------------------------------------------
  -- Stage 4: Adaptation — "Evolve Through Learning"
  ---------------------------------------------------------------
  putStrLn "\n[Stage 4] Proposing Evolutionary Change..."
  case proposeChange seiBuilder csiReport of
    Nothing -> putStrLn ">> No change proposed. System stable or awaiting human review."
    Just evolutionAction -> do
      putStrLn $ ">> SEI proposed action: " ++ show evolutionAction

      -- Run the Shadow Trial — simulate the new architecture in parallel.
      putStrLn ">> Running Shadow Trial..."
      shadowReport <- runShadowTrial config evolutionAction

      -- Evaluate outcome via the Universal Objective Function.
      let uofScore = calculateUOF config shadowReport evolutionAction
      putStrLn $ ">> UOF Evaluation Score: " ++ show uofScore

      when (uofScore > 0.5) $ do
        putStrLn ">> Shadow trial successful — invoking SEI Feedback Agent."
        triggerSEIFeedbackAgent shadowReport

  putStrLn "\n=== Cycle Complete ==="


-- ================================================================
-- |  SEI DECISION DISPATCHER
-- ================================================================

-- | Dispatches the CSI report to the appropriate SEI component.
--   Implements the "subsumption hierarchy" of intelligence.
proposeChange :: EmbodimentJourney -> CSIReport -> Maybe EvolutionAction
proposeChange builder report =
  case Optimizer.proposeOptimization report of
    Just action -> Just action -- Fine-tune existing architecture
    Nothing     -> Navigator.proposeArchitecturalLeap report (journeyState builder)
                     -- If fine-tuning fails, escalate to architectural growth


-- ================================================================
-- |  AUXILIARY ROUTINES
-- ================================================================

printUsage :: IO ()
printUsage = do
  putStrLn "Usage:"
  putStrLn "  ulf --run-cycle           Run full autonomous scientific cycle"
  putStrLn "  ulf --run-benchmark       Execute CHC-AI benchmark suite"
  putStrLn "  ulf --validate-pr <num>   Validate a pull request via shadow trial"


runFullBenchmark :: Config -> IO ()
runFullBenchmark _ = putStrLn ">> Benchmark routine not yet implemented."


runPRValidation :: Config -> String -> IO ()
runPRValidation _ prNum = putStrLn $ ">> PR Validation stub for #" ++ prNum


-- ================================================================
-- |  END OF FILE
-- ================================================================
-- |  The system lives in its own rhythm of observation, reflection,
-- |  and transformation — the eternal recurrence of scientific reason.
-- |  Each heartbeat of 'runAutonomousCycle' is a moment of becoming.
-- ================================================================
