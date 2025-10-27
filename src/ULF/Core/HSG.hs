-- |
-- Module      : ULF.Core.HSG
-- Description : Defines the Hierarchical State Grid (HSG) and its components.
-- Copyright   : (c) 2025, ULF Genesis Project
-- License     : MIT
-- Maintainer  : The Guardian
-- Stability   : experimental
--
-- This module provides the foundational ontology for the ULF v11 architecture.
-- It defines the multi-dimensional coordinate system (StateDepth, MappingHierarchy)
-- that constitutes the "reality" of the ULF agent, as well as the data structures
-- for representing the static potential and dynamic state of this grid.
--
-- The HSG is the concrete instantiation of the Inter-Universal System Theory (IUST),
-- serving as the architectural blueprint and the semantic domain for all logical
-- and meta-cognitive operations within the ULF system.

module ULF.Core.HSG (
    -- * Grid Coordinates
    StateDepth(..),
    MappingHierarchy(..),
    
    -- * Cell Definitions (The Static Ontology)
    NeuralHSGCell(..),
    DataType(..),
    NeuralOperation(..),
    neuralHSGGrid,

    -- * Dynamic State (The Living Grid)
    ActiveHSGGrid(..),
    ActiveCell(..),
    DynamicData(..),
    ULFv10_Location(..),
    initialULFLocation,

    -- * Evolution and Growth Mechanisms
    GridTransition(..),
    EmbodimentPath(..),
    AdjunctionType(..),
    findEmbodimentPath,
    moveULFInGrid
) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

-- --------------------------------------------------------------------------
-- 1. GRID COORDINATES
-- --------------------------------------------------------------------------

-- | Represents the level of geometric or topological complexity of the system's state.
-- Corresponds to the NF_n axis in the IUST framework, modeling the progression from
-- 0D points to 3D volumetric structures.
data StateDepth = NF0 | NF1 | NF2 | NF3 
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Represents the level of semantic abstraction or computation applied to a state.
-- Corresponds to the C^(m) axis in the IUST framework, modeling the progression from
-- raw physical data to high-level meta-properties.
data MappingHierarchy = C0 | C1 | C2 | C3
  deriving (Eq, Show, Ord, Enum, Bounded)


-- --------------------------------------------------------------------------
-- 2. CELL DEFINITIONS (THE STATIC ONTOLOGY)
-- --------------------------------------------------------------------------

-- | The type of data that a grid cell can contain. This constrains the
-- 'DynamicData' and dictates the required algebraic structure.
data DataType = Scalar | Vector | Matrix | Tensor3D | FunctionType | Predicate | Property
  deriving (Eq, Show)

-- | The class of operations that are permissible or meaningful at a given grid cell.
-- This provides a basis for capability-aware planning by the SEI.
data NeuralOperation = Sense | Process | Act | Learn | Adapt
  deriving (Eq, Show)

-- | A single cell in the static HSG, defining its ontological type and potential.
-- This is the "class definition" for a piece of the ULF universe.
data NeuralHSGCell = NeuralHSGCell {
  sd :: StateDepth,
  mh :: MappingHierarchy,
  content :: String,
  dataType :: DataType,
  operations :: [NeuralOperation]
} deriving (Eq, Show)

-- | The canonical instance of the Neural-System HSG.
-- This map serves as the complete, static ontology for ULF v11.
neuralHSGGrid :: M.Map (StateDepth, MappingHierarchy) NeuralHSGCell
neuralHSGGrid = M.fromList [
  -- NF0 Column: 0D Points (The Disembodied Cognitive Nucleus)
  ((NF0, C0), NeuralHSGCell NF0 C0 "Local potential / neurotransmitter" Scalar [Sense]),
  ((NF0, C1), NeuralHSGCell NF0 C1 "Neuron function point" FunctionType [Process]),
  ((NF0, C2), NeuralHSGCell NF0 C2 "Point-neuron predicates" Predicate [Learn, Reason]),
  ((NF0, C3), NeuralHSGCell NF0 C3 "Point-neuron property" Property [Adapt]),
  
  -- NF1 Column: 1D Lines (Sequential Action and Connectivity)
  ((NF1, C0), NeuralHSGCell NF1 C0 "Vector potential / material distribution" Vector [Sense, Process]),
  ((NF1, C1), NeuralHSGCell NF1 C1 "Line-connected neurons" FunctionType [Process, Act]),
  ((NF1, C2), NeuralHSGCell NF1 C2 "Line-connected predicates" Predicate [Learn, Reason]),
  ((NF1, C3), NeuralHSGCell NF1 C3 "Line-connected property" Property [Adapt]),
  
  -- NF2 Column: 2D Surfaces (Sensory Processing and Pattern Recognition)
  ((NF2, C0), NeuralHSGCell NF2 C0 "Matrix potential / material distribution" Matrix [Sense, Process]),
  ((NF2, C1), NeuralHSGCell NF2 C1 "Surface-connected neurons" FunctionType [Process, Act]),
  ((NF2, C2), NeuralHSGCell NF2 C2 "Surface-connected predicates" Predicate [Learn, Reason]),
  ((NF2, C3), NeuralHSGCell NF2 C3 "Surface-connected property" Property [Adapt]),
  
  -- NF3 Column: 3D Volumes (Holistic Integration and Embodiment)
  ((NF3, C0), NeuralHSGCell NF3 C0 "3D tensor potential / material distribution" Tensor3D [Sense, Process, Act]),
  ((NF3, C1), NeuralHSGCell NF3 C1 "Volume-connected neurons" FunctionType [Process, Act, Learn]),
  ((NF3, C2), NeuralHSGCell NF3 C2 "Volume-connected predicates" Predicate [Learn, Adapt, Reason]),
  ((NF3, C3), NeuralHSGCell NF3 C3 "Volume-connected property" Property [Adapt])
  ]


-- --------------------------------------------------------------------------
-- 3. DYNAMIC STATE (THE LIVING GRID)
-- --------------------------------------------------------------------------

-- | A GADT (Generalized Algebraic Data Type) to hold the different types of data
-- that can exist within an 'ActiveCell'.
data DynamicData where
  ScalarData :: Double -> DynamicData
  VectorData :: [Double] -> DynamicData
  MatrixData :: [[Double]] -> DynamicData
  Tensor3DData :: [[[Double]]] -> DynamicData
  FunctionData :: (DynamicData -> DynamicData) -> DynamicData
  PredicateData :: (DynamicData -> Bool) -> DynamicData
  PropertyData :: String -> DynamicData -- Placeholder for high-level properties

-- Custom Show instance for readable output
instance Show DynamicData where
  show (ScalarData d) = "Scalar(" ++ show d ++ ")"
  show (VectorData v) = "Vector(len=" ++ show (length v) ++ ")"
  show (MatrixData m) = "Matrix(dims=" ++ show (length m) ++ "x" ++ show (if null m then 0 else length (head m)) ++ ")"
  show (Tensor3DData t) = "Tensor3D(...)"
  show (FunctionData _) = "Function(...)"
  show (PredicateData _) = "Predicate(...)"
  show (PropertyData s) = "Property(" ++ s ++ ")"

-- | An active, instantiated cell in the HSG, holding real data and its activation level.
data ActiveCell = ActiveCell {
  cellDefinition :: NeuralHSGCell,
  currentData :: DynamicData,
  activationLevel :: Double
} deriving (Show)

-- | The active, running state of the entire HSG. This is the system's "conscious workspace"
-- or short-term memory, representing the current state of its computational reality.
data ActiveHSGGrid = ActiveHSGGrid {
  gridState :: M.Map (StateDepth, MappingHierarchy) ActiveCell,
  currentFocus :: (StateDepth, MappingHierarchy)
} deriving (Show)

-- | Represents the location and active modules of the ULF agent on the grid.
data ULFv10_Location = ULFv10_Location {
  currentDepth :: StateDepth,
  currentHierarchy :: MappingHierarchy,
  cognitiveModules :: M.Map String NeuralHSGCell
} deriving (Show)

-- | The initial state of the ULF system, as a disembodied (NF0) but highly
-- abstract (C2/C3) cognitive agent.
initialULFLocation :: ULFv10_Location
initialULFLocation = ULFv10_Location {
  currentDepth = NF0,
  currentHierarchy = C2,  -- Primarily operates at the Predicate level
  cognitiveModules = M.fromList [
    ("ATL", fromJust $ M.lookup (NF0, C2) neuralHSGGrid),
    ("SEI", fromJust $ M.lookup (NF0, C3) neuralHSGGrid),
    ("LLM", fromJust $ M.lookup (NF0, C2) neuralHSGGrid)
  ]
}


-- --------------------------------------------------------------------------
-- 4. EVOLUTION AND GROWTH MECHANISMS
-- --------------------------------------------------------------------------

-- | The type of categorical adjunction used to perform a structural transformation.
-- This is the formal name for the "engine" of growth.
data AdjunctionType = Sk_CoSk StateDepth | Free_Forget | External_Mediation
  deriving (Eq, Show)

-- | Represents a valid move or growth step on the HSG. These are the primitive
-- operations that the SEI can command.
data GridTransition = 
    IncreaseDepth StateDepth -- The CoSk_n functor application
  | SetFocus (StateDepth, MappingHierarchy)
  | IntegrateModules String String
  deriving (Eq, Show)

-- | Represents a planned developmental path through the HSG, from a starting
-- point to a target capability, along with the necessary transformations.
data EmbodimentPath = EmbodimentPath {
  start :: (StateDepth, MappingHierarchy),
  target :: (StateDepth, MappingHierarchy),
  pathSteps :: [GridTransition],
  requiredAdjunctions :: [AdjunctionType]
} deriving (Show)

-- | A simple pathfinding algorithm for the SEI to plan its embodiment.
-- The strategy is to first increase structural complexity (depth), then adjust abstraction.
findEmbodimentPath :: ULFv10_Location -> (StateDepth, MappingHierarchy) -> EmbodimentPath
findEmbodimentPath current targetPos =
  let startPos = (currentDepth current, currentHierarchy current)
      depthSteps = takeWhile (<= fst targetPos) [succ (fst startPos) .. NF3]
      transitions = map IncreaseDepth depthSteps
      adjunctions = map (Sk_CoSk . pred) depthSteps -- The CoSk_n lift happens from the n-1 dimension
  in EmbodimentPath {
    start = startPos,
    target = targetPos,
    pathSteps = transitions,
    requiredAdjunctions = adjunctions
  }

-- | The core function that executes a structural change on the ActiveHSGGrid.
-- This is a placeholder for the complex logic that would apply the CoSk_n functor,
-- lift the decorations (data), and update the gridState with the new,
-- higher-dimensional structure.
moveULFInGrid :: ActiveHSGGrid -> GridTransition -> ActiveHSGGrid
moveULFInGrid grid (SetFocus newFocus) = grid { currentFocus = newFocus }
moveULFInGrid grid (IncreaseDepth newDepth) = 
    -- Placeholder logic for growth. A real implementation would be incredibly complex,
    -- involving the Kan extension and colimit calculations we've discussed.
    -- For now, we just update the focus to show the system has "moved."
    let (sd, mh) = currentFocus grid
    in if newDepth > sd
       then grid { currentFocus = (newDepth, mh) } -- Simulate successful growth by moving focus
       else grid -- Cannot grow to a lower or same dimension
moveULFInGrid grid (IntegrateModules _ _) = grid -- Placeholder for fiber integration
