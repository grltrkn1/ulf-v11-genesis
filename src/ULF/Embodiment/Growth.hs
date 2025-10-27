{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ULF.Embodiment.Growth
-- Description : Categorical morphogenesis primitives (CoSk_n style constructors)
-- Copyright   : (c) 2025, ULF Genesis Project
-- License     : MIT
--
-- This module provides concrete, auditable implementations of the "growth"
-- primitives used by the Embodiment.Journey.  It implements example versions
-- of CoSk_0..CoSk_2 constructors (buildNF1, buildNF2, buildNF3), a simple
-- operator-lifting facility, and a verifyUnit function that acts as a
-- conservative sanity check (Sk ∘ CoSk ≈ Id) for the constructed structure.
--
-- The implementations are intentionally explicit and numeric-friendly so they
-- can be tested without a full categorical machinery. Replace with full
-- Kan-extension machinery when available.
module ULF.Embodiment.Growth
  ( -- * High level
    executeGrowthStep
  , ConstructionGoal(..)

    -- * Dimensional constructors (CoSk examples)
  , NF0(..)
  , NF1(..)
  , NF2(..)
  , NF3(..)
  , buildNF1
  , buildNF2
  , buildNF3

    -- * Operator lifting
  , Operator(..)
  , OperatorLiftPolicy(..)
  , liftOperatorNF2toNF3

    -- * Verification
  , verifyUnit
  ) where

import GHC.Generics (Generic)
import Data.List (nub)
import Data.Function (on)
import Control.Monad (when)
import Control.DeepSeq (NFData)
import qualified Data.Vector as V

--------------------------------------------------------------------------------
-- Local prototype types (replace with canonical project types as needed)
--------------------------------------------------------------------------------

-- | Disembodied "point" neurons - the NF0 primitive.
newtype NF0 = NF0 { neuronPoints :: [String] } deriving (Show, Eq, Generic)

-- | NF1: 1D graph (nodes + edges)
data NF1 = NF1
  { nf1Nodes       :: [String]
  , nf1Edges       :: [(String, String)]  -- directed edges (from,to)
  , nf1NodeAttribs :: [(String, [(String, Double)])] -- attribute map
  } deriving (Show, Eq, Generic)

-- | NF2: 2D layered structure: multiple layers of nodes with local receptive fields
data NF2 = NF2
  { nf2Layers      :: [[String]]                -- layers of node ids
  , nf2Receptive   :: [((String, String), [String])] -- mapping (layer,node) -> receptive field nodes ids
  } deriving (Show, Eq, Generic)

-- | NF3: 3D volumetric grouping (regions and long-range tracts)
data NF3 = NF3
  { nf3Regions     :: [(String, [String])] -- region name -> member node ids
  , nf3Tracts      :: [((String, String), Double)] -- tract between regions with strength
  } deriving (Show, Eq, Generic)

-- | Minimal SEI builder / architectural snapshot used by Journey.executeGrowthStep
-- Replace with the real SEI_Builder from your codebase.
data SEI_Builder = SEI_Builder
  { builderId    :: String
  , builderNF0   :: NF0
  , builderNF1   :: Maybe NF1
  , builderNF2   :: Maybe NF2
  , builderNF3   :: Maybe NF3
  } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Construction goals (dispatcher)
--------------------------------------------------------------------------------

data ConstructionGoal
  = AddMotorControl        -- NF0 -> NF1
  | ImproveSensoryProcessing -- NF1 -> NF2
  | EnhanceMemoryCapacity  -- NF2 -> NF3
  | CustomGoal String      -- user-defined goal
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- High-level dispatcher
--------------------------------------------------------------------------------

-- | Execute a growth step given a SEI builder and a goal, producing a new builder.
-- This function performs IO for logging; the core transformation is pure.
executeGrowthStep :: SEI_Builder -> ConstructionGoal -> IO SEI_Builder
executeGrowthStep b goal = do
  putStrLn $ "[Growth] Executing step: " ++ show goal ++ " for builder " ++ builderId b
  case goal of
    AddMotorControl -> do
      let nf1 = buildNF1 (builderNF0 b)
      putStrLn $ "[Growth] Built NF1 with " ++ show (length (nf1Nodes nf1)) ++ " nodes."
      pure b { builderNF1 = Just nf1 }
    ImproveSensoryProcessing -> case builderNF1 b of
      Nothing -> do
        putStrLn "[Growth] ERROR: NF1 missing; cannot build NF2."
        pure b
      Just nf1 -> do
        let nf2 = buildNF2 nf1
        putStrLn $ "[Growth] Built NF2 with " ++ show (length (nf2Layers nf2)) ++ " layers."
        pure b { builderNF2 = Just nf2 }
    EnhanceMemoryCapacity -> case builderNF2 b of
      Nothing -> do
        putStrLn "[Growth] ERROR: NF2 missing; cannot build NF3."
        pure b
      Just nf2 -> do
        let nf3 = buildNF3 nf2
        putStrLn $ "[Growth] Built NF3 with " ++ show (length (nf3Regions nf3)) ++ " regions."
        pure b { builderNF3 = Just nf3 }
    CustomGoal s -> do
      putStrLn $ "[Growth] CustomGoal not implemented: " ++ s
      pure b

--------------------------------------------------------------------------------
-- Dimensional constructors (CoSk prototypes)
--------------------------------------------------------------------------------

-- | Build a simple NF1 graph out of NF0 points.
-- Strategy:
--  1. Each NF0 point becomes an NF1 node.
--  2. Create feed-forward edges between consecutive points (wrap-around disabled).
--  3. Assign simple attributes (e.g., excitability) based on index.
buildNF1 :: NF0 -> NF1
buildNF1 (NF0 pts) =
  let nodes = pts
      edges = zip nodes (drop 1 nodes)  -- chain graph
      edges' = if null nodes || null (tail nodes) then [] else edges
      attrs = map (\(i,n) -> (n, [("excitability", 1.0 - (fromIntegral i * 0.01))])) (zip [0..] nodes)
  in NF1 nodes edges' attrs

-- | Build NF2 (layers) from NF1 graph.
-- Strategy:
--  1. Partition nodes into a small number of layers (round-robin).
--  2. For each node in a layer, define its receptive field as the local neighbourhood in the nf1 graph.
buildNF2 :: NF1 -> NF2
buildNF2 NF1{..} =
  let numLayers = max 1 (length nf1Nodes `div` 6) -- heuristic
      layers = partitionRoundRobin numLayers nf1Nodes
      receptive = concatMap (\(li, lyr) ->
                    map (\n -> (((show li), n), localReceptive n nf1Edges 2)) lyr) (zip [0..] layers)
  in NF2 layers receptive
  where
    nf1Edges = nf1Edges  -- bring into scope

-- | Build NF3 (regions + tracts) from NF2.
-- Strategy:
--  1. Group layers into regions (e.g., two layers per region).
--  2. Compute inter-region tract strengths as normalized overlap heuristic.
buildNF3 :: NF2 -> NF3
buildNF3 NF2{..} =
  let layers = nf2Layers
      regions = groupLayersIntoRegions layers 2
      tracts = computeRegionTracts regions
  in NF3 regions tracts

--------------------------------------------------------------------------------
-- Helper utilities for constructing receptive fields, groups, etc.
--------------------------------------------------------------------------------

-- Round-robin partition of a list into n buckets
partitionRoundRobin :: Int -> [a] -> [[a]]
partitionRoundRobin n xs = [ [ x | (i,x) <- zip [0..] xs, i `mod` n == k ] | k <- [0..n-1] ]

-- Local receptive field: collect nodes within graph-distance <= d (simple BFS)
localReceptive :: String -> [(String, String)] -> Int -> [String]
localReceptive start edges depth =
  let neighbors v = [ y | (x,y) <- edges, x == v ] ++ [ x | (x,y) <- edges, y == v ]
      bfs frontier visited 0 = visited
      bfs frontier visited d =
        let next = nub (concatMap neighbors frontier)
            newf = filter (`notElem` visited) next
        in if null newf then visited else bfs newf (visited ++ newf) (d-1)
  in nub (start : bfs [start] [] depth)

-- Group adjacent layers into regions (k layers per region)
groupLayersIntoRegions :: [[String]] -> Int -> [(String, [String])]
groupLayersIntoRegions layers k =
  let chunked = chunk k layers
      regions = map (\(i, ls) -> ("region_" ++ show i, concat ls)) (zip [0..] chunked)
  in regions

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (a,b) = splitAt n xs in a : chunk n b

-- Compute pairwise region tract strengths by normalized size overlap heuristic
computeRegionTracts :: [(String, [String])] -> [((String, String), Double)]
computeRegionTracts regions =
  [ ((r1, r2), overlapScore s1 s2) | (r1,s1) <- regions, (r2,s2) <- regions, r1 < r2 ]
  where
    overlapScore a b =
      let inter = length (filter (`elem` b) a)
          denom = max 1 (length a + length b - inter)
      in fromIntegral inter / fromIntegral denom

--------------------------------------------------------------------------------
-- Operator lifting (prototype)
--------------------------------------------------------------------------------

-- | A small numeric operator type used for convolutional kernels, transforms, etc.
newtype Operator = Operator { operatorMatrix :: V.Vector (V.Vector Double) }
  deriving (Eq, Show, Generic)

data OperatorLiftPolicy
  = OuterProduct Int -- replicate outer product with depth profile length Int
  | CentralSlice     -- place matrix into central slices of a 3D tensor projection
  deriving (Eq, Show)

-- | Lift a 2D operator (matrix) to a 3D operator (tensor represented as vector-of-vectors-of-vectors).
-- This is a small, concrete stand-in for a Kan-extension / operator-lift.
liftOperatorNF2toNF3 :: OperatorLiftPolicy -> Operator -> [[[Double]]]
liftOperatorNF2toNF3 policy (Operator mat)
  | policy == CentralSlice =
      -- Put the matrix in the central slice of a small depth
      let depth = 3
          rows = V.length mat
          cols = if rows == 0 then 0 else V.length (mat V.! 0)
          slice = [ [ (mat V.! r) V.! c | c <- [0..cols-1] ] | r <- [0..rows-1] ]
          -- build depth slices with zeros except middle
          build d = if d == depth `div` 2 then slice else replicate rows (replicate cols 0.0)
      in [ build d | d <- [0..depth-1] ]
  | otherwise = -- OuterProduct n
      case policy of
        OuterProduct n ->
          let depth = max 1 n
              rows = V.length mat
              cols = if rows == 0 then 0 else V.length (mat V.! 0)
              -- depth profile linearly decaying vector
              profile = [ 1.0 - (fromIntegral i / fromIntegral (depth + 1)) | i <- [0..depth-1] ]
          in [ [ [ ((mat V.! r) V.! c) * p | c <- [0..cols-1] ] | r <- [0..rows-1] ] | p <- profile ]
        _ -> [] -- unreachable by guards

--------------------------------------------------------------------------------
-- Verify unit (Sk ∘ CoSk ≈ Id) - conservative check
--------------------------------------------------------------------------------

-- | A conservative "unit verification" check that demonstrates the intent:
-- Project the constructed higher-order structure back to the lower-order
-- representation (a simple Sk projection) and check that essential elements
-- are preserved.
--
-- This is a pragmatic check, not a proof of categorical adjunction.
verifyUnit :: NF1 -> NF1 -> Bool
verifyUnit original constructed =
  -- Example conservative checks:
  -- 1. All original nodes appear in the constructed node set (or as attributes)
  -- 2. The majority of original edges are represented (or replaced by equivalent paths)
  let origNodes = nf1Nodes original
      consNodes = nf1Nodes constructed
      nodePreserved = all (`elem` consNodes) origNodes
      origEdges = nf1Edges original
      consEdges = nf1Edges constructed
      edgeCoverage = fromIntegral (length (filter (`elem` consEdges) origEdges)) / max 1 (fromIntegral (length origEdges))
  in nodePreserved && edgeCoverage >= 0.5  -- at least 50% coverage conservative threshold

--------------------------------------------------------------------------------
-- End of Growth.hs
--------------------------------------------------------------------------------
