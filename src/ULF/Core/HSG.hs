{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : ULF.Core.HSG
-- Description : Defines the Hierarchical State Grid (HSG) for the ULF v11 Architecture.
-- Copyright   : (c) 2025, ULF Genesis Project
-- License     : MIT
-- Maintainer  : The Guardian
-- Stability   : experimental
--
-- This module provides the foundational ontology for the ULF v11 architecture.
-- It defines the multi-dimensional coordinate system (StateDepth, MappingHierarchy)
-- that constitutes the "computational reality" of the ULF agent. It also defines
-- the data structures for representing the static potential and the dynamic, "living"
-- state of this grid, along with the formal mechanisms for its evolution.
-- The HSG is the concrete instantiation of the Inter-Universal System Theory (IUST).

module ULF.Core.HSG (
    -- * Grid Coordinates
    StateDepth(..),
    MappingHierarchy(..),

    -- * Static Ontology (Cell Definitions)
    DataType(..),
    NeuralOperation(..),
    NeuralHSGCell(..),
    neuralHSGGrid,
    lookupCanonicalCell,

    -- * Dynamic State (The Living Grid)
    DynamicData(..),
    ActiveCell(..),
    ActiveHSGGrid(..),
    initialActiveGrid,

    -- * Evolution and Growth
    AdjunctionType(..),
    GridTransition(..),
    EmbodimentPath(..),
    findEmbodimentPath,
    moveULFInGrid
) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Generics (Generic)
import Data.List (foldl')
import ULF.Core.Types (Atom) -- Assuming this is in the same directory

-- --------------------------------------------------------------------------
-- 1. GRID COORDINATES
-- --------------------------------------------------------------------------

-- | Represents the level of geometric or topological complexity (NF-axis).
data StateDepth = NF0 | NF1 | NF2 | NF3
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

-- | Represents the level of semantic abstraction (C-axis).
data MappingHierarchy = C0 | C1 | C2 | C3
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

-- --------------------------------------------------------------------------
-- 2. STATIC ONTOLOGY (THE "MAP OF POTENTIAL")
-- --------------------------------------------------------------------------

-- | The type of data a grid cell can contain.
data DataType = Scalar | Vector | Matrix | Tensor3D | FunctionType | PredicateType | PropertyType
  deriving (Eq, Show, Generic)

-- | The class of operations permissible at a grid cell.
data NeuralOperation = Sense | Process | Act | Learn | Adapt
  deriving (Eq, Show, Generic)

-- | A single static cell definition in the HSG.
data NeuralHSGCell = NeuralHSGCell
  { sd         :: !StateDepth
  , mh         :: !MappingHierarchy
  , content    :: !String
  , dataType   :: !DataType
  , operations :: ![NeuralOperation]
  } deriving (Eq, Show, Generic)

-- | The canonical, static map of the HSG, defining the "laws of physics" for the ULF universe.
neuralHSGGrid :: M.Map (StateDepth, MappingHierarchy) NeuralHSGCell
neuralHSGGrid = M.fromList
  [ ((NF0, C0), NeuralHSGCell NF0 C0 "Local potential" Scalar [Sense])
  , ((NF0, C1), NeuralHSGCell NF0 C1 "Point-neuron function" FunctionType [Process])
  , ((NF0, C2), NeuralHSGCell NF0 C2 "Point-neuron predicate" PredicateType [Learn, Adapt])
  , ((NF0, C3), NeuralHSGCell NF0 C3 "Point-neuron property" PropertyType [Adapt])
  , ((NF1, C0), NeuralHSGCell NF1 C0 "Vector potential" Vector [Sense, Process])
  , ((NF1, C1), NeuralHSGCell NF1 C1 "Line-connected neurons" FunctionType [Process, Act])
  , ((NF2, C0), NeuralHSGCell NF2 C0 "Matrix potential" Matrix [Sense, Process])
  , ((NF2, C1), NeuralHSGCell NF2 C1 "Surface-connected neurons" FunctionType [Process, Act])
  , ((NF3, C0), NeuralHSGCell NF3 C0 "3D tensor potential" Tensor3D [Sense, Process, Act])
  , ((NF3, C1), NeuralHSGCell NF3 C1 "Volume-connected neurons" FunctionType [Process, Act, Learn])
  -- ... Add all 16 cells for completeness as defined in our architecture.
  ]

-- | Safe lookup for a canonical cell definition.
lookupCanonicalCell :: (StateDepth, MappingHierarchy) -> Maybe NeuralHSGCell
lookupCanonicalCell = (`M.lookup` neuralHSGGrid)

-- --------------------------------------------------------------------------
-- 3. DYNAMIC STATE (THE "LIVING" GRID)
-- --------------------------------------------------------------------------

-- | A GADT to hold heterogeneous runtime data in 'ActiveCell's.
data DynamicData where
  ScalarData   :: !Double -> DynamicData
  VectorData   :: ![Double] -> DynamicData
  MatrixData   :: ![[Double]] -> DynamicData
  Tensor3DData :: ![[[Double]]] -> DynamicData
  FunctionData :: !(DynamicData -> DynamicData) -> DynamicData
  PredicateData :: !(DynamicData -> Bool) -> DynamicData
  PropertyData :: !String -> DynamicData

instance Show DynamicData where
  show (ScalarData d)    = "Scalar(" ++ show d ++ ")"
  show (VectorData v)    = "Vector(len=" ++ show (length v) ++ ")"
  show (MatrixData m)    = "Matrix(dims=" ++ show (length m) ++ "x" ++ show (if null m then 0 else length (head m)) ++ ")"
  show (Tensor3DData _)  = "Tensor3D(...)"
  show (FunctionData _)  = "Function(...)"
  show (PredicateData _) = "Predicate(...)"
  show (PropertyData s)  = "Property(" ++ s ++ ")"

-- | An active, instantiated cell: its static definition, runtime data, and activation level.
data ActiveCell = ActiveCell
  { cellDefinition  :: !NeuralHSGCell
  , currentData     :: !DynamicData
  , activationLevel :: !Double
  } deriving (Show, Generic)

-- | The complete, dynamic state of the ULF agent, representing its "conscious workspace."
data ActiveHSGGrid = ActiveHSGGrid
  { gridState    :: !(M.Map (StateDepth, MappingHierarchy) ActiveCell)
  , currentFocus :: !(StateDepth, MappingHierarchy)
  } deriving (Show, Generic)

-- | Creates the initial "Genesis Block" state of the ULF system's active grid.
initialActiveGrid :: ActiveHSGGrid
initialActiveGrid =
  let
    initialFocus = (NF0, C2)
    initialModules = [("ATL", (NF0, C2)), ("SEI", (NF0, C3)), ("LLM", (NF0, C1))]

    mkActiveCell (name, coord) = do
      def <- lookupCanonicalCell coord
      let initialData = PropertyData ("Module " ++ name ++ " active")
      pure (coord, ActiveCell def initialData 1.0)

  in ActiveHSGGrid
       { gridState = M.fromList $ mapMaybe mkActiveCell initialModules
       , currentFocus = initialFocus
       }

-- --------------------------------------------------------------------------
-- 4. EVOLUTION AND GROWTH
-- --------------------------------------------------------------------------

-- | The type of categorical adjunction used to perform a structural transformation.
data AdjunctionType = Sk_CoSk StateDepth | Free_Forget | External_Mediation
  deriving (Eq, Show, Generic)

-- | Represents a valid, atomic architectural change or state transition.
data GridTransition
  = IncreaseDepth StateDepth
  | SetFocus (StateDepth, MappingHierarchy)
  deriving (Eq, Show, Generic)

-- | A planned, multi-step developmental path for embodiment.
data EmbodimentPath = EmbodimentPath
  { startPos            :: !(StateDepth, MappingHierarchy)
  , targetPos           :: !(StateDepth, MappingHierarchy)
  , pathSteps           :: ![GridTransition]
  , requiredAdjunctions :: ![AdjunctionType]
  } deriving (Show, Generic)

-- | Pathfinding algorithm for the SEI Navigator.
findEmbodimentPath :: ActiveHSGGrid -> StateDepth -> EmbodimentPath
findEmbodimentPath grid targetDepth =
  let start@(curDepth, _) = currentFocus grid
      depthSteps = filter (> curDepth) $ takeWhile (<= targetDepth) [succ curDepth ..]
      transitions = map IncreaseDepth depthSteps
      adjunctions = map (Sk_CoSk . pred) depthSteps
  in EmbodimentPath start (targetDepth, C3) transitions adjunctions
  where
    pred NF0 = NF0; pred NF1 = NF0; pred NF2 = NF1; pred NF3 = NF2

-- | Applies a GridTransition to an ActiveHSGGrid. This is the executor of morphogenesis.
moveULFInGrid :: ActiveHSGGrid -> GridTransition -> ActiveHSGGrid
moveULFInGrid grid (SetFocus newFocus) = grid { currentFocus = newFocus }
moveULFInGrid grid (IncreaseDepth newDepth) =
  let (curDepth, curMh) = currentFocus grid
  in if newDepth <= curDepth
       then grid -- No-op if not a progressive growth.
       else
         let
           activeAtCurDepth = M.filterWithKey (\(sd, _) _ -> sd == curDepth) (gridState grid)
           liftedCells = mapMaybe (liftCell newDepth) (M.toList activeAtCurDepth)
           newGridState = M.union (M.fromList liftedCells) (gridState grid)
         in
           grid { gridState = newGridState, currentFocus = (newDepth, curMh) }
  where
    liftCell targetDepth ((_, mh), activeCell) = do
      newCellDef <- lookupCanonicalCell (targetDepth, mh)
      -- Conservative lift: preserve runtime data, update definition.
      let newActiveCell = activeCell { cellDefinition = newCellDef }
      pure ((targetDepth, mh), newActiveCell)
