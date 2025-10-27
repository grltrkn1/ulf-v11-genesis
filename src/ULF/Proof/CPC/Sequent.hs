{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ULF.Proof.CPC.Sequent
-- Description : Sequent calculus prover (basic backward search) for CPC
-- Copyright   : (c) 2025, ULF Genesis Project
-- License     : MIT
--
-- A compact sequent prover that constructs ProofTree when Γ ⊢ Δ is derivable.
-- Uses depth-limited backward search and a small set of sequent rules.
module ULF.Proof.CPC.Sequent
  ( -- * Data types
    Sequent(..)
  , ProofTree(..)

    -- * Prover
  , prove
  , proveWithLimit

    -- * Utilities
  , prettySequent
  ) where

import GHC.Generics (Generic)
import Data.List (delete, partition, nub)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as S

-- Import the CPC AST (syntax) from the CPC institution module.
-- Assumes `ULF.Institutions.CPC` exports `CPCSentence(..)`.
import ULF.Institutions.CPC (CPCSentence(..))

--------------------------------------------------------------------------------
-- Sequents and Proof Trees
--------------------------------------------------------------------------------

-- | A two-sided sequent Γ ⊢ Δ where gamma (antecedent) and delta (succedent)
-- are lists of formulas. Read: conjunction of Γ entails disjunction of Δ.
data Sequent = Sequent
  { gamma :: [CPCSentence]
  , delta :: [CPCSentence]
  } deriving (Eq, Show, Generic)

-- | A proof tree: either an axiom leaf or an inference with named rule.
data ProofTree
  = Axiom Sequent
  | Inference
      { ruleName   :: String
      , conclusion :: Sequent
      , premises   :: [ProofTree]
      }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Top-level prover (depth-limited)
--------------------------------------------------------------------------------

-- | Try to prove a sequent with a reasonable default depth limit.
-- Returns Just ProofTree when derivable, Nothing otherwise.
prove :: Sequent -> Maybe ProofTree
prove = proveWithLimit 120

-- | Try to prove a sequent with an explicit depth limit.
-- This uses a backward-search style: decompose the goal into premises.
proveWithLimit :: Int -> Sequent -> Maybe ProofTree
proveWithLimit limit goal = search limit S.empty goal

-- Internal search with visited set to avoid obvious loops.
-- visited is a set of stringified sequents.
search :: Int -> S.Set String -> Sequent -> Maybe ProofTree
search depth visited seqnt
  | depth <= 0 = Nothing
  | axiomCheck seqnt = Just (Axiom seqnt)
  | S.member key visited = Nothing
  | otherwise =
      -- try rules in a prioritized order
      let visited' = S.insert key visited
          attempts = ruleApplications seqnt
          try [] = Nothing
          try ((name, premisesF):xs) =
            -- premisesF :: [Sequent]
            let attempt = mapM (search (depth - 1) visited') premisesF
            in case attempt of
                 Just pts -> Just (Inference name seqnt pts)
                 Nothing  -> try xs
      in try attempts
  where
    key = showSequent seqnt

--------------------------------------------------------------------------------
-- Axiom check: if some formula appears both on left and right as identical atomic
-- formula, it's an axiom. We also treat tautological syntactic appearances such as
-- having ⊥ on the right or ⊤ on the left if those were present (not modeled here).
axiomCheck :: Sequent -> Bool
axiomCheck (Sequent gs ds) =
  -- If any formula in gamma syntactically equals some formula in delta, treat as axiom.
  not . null $ [ () | g <- gs, d <- ds, syntacticAtomEq g d ]

-- syntactic atom equality: treat CPCVar identical; other formulas only equal if exactly equal
syntacticAtomEq :: CPCSentence -> CPCSentence -> Bool
syntacticAtomEq (CPCVar a) (CPCVar b) = a == b
syntacticAtomEq x y = x == y

--------------------------------------------------------------------------------
-- Rule applications (backward): for a given sequent, produce a list of
-- (ruleName, premises) options to try (each premises list is a conjunction
-- of sequents that must all be proved).
--
-- This implements a small set of standard sequent rules for CPC.
--------------------------------------------------------------------------------

-- Composes possible rule applications in prioritized order.
ruleApplications :: Sequent -> [(String, [Sequent])]
ruleApplications s =
  -- Prefer decomposing complex succedent (right rules), then antecedent (left rules).
  concat
    [ applyRightRules s
    , applyLeftRules s
    , structuralRules s
    ]

--------------------------------------------------------------------------------
-- RIGHT rules (decompose formulas in succedent; backward step)
--------------------------------------------------------------------------------

-- If Δ contains A ∧ B, to prove Γ ⊢ ... A∧B, ... we must prove Γ ⊢ ..., A, ... and Γ ⊢ ..., B, ...
applyRightRules :: Sequent -> [(String, [Sequent])]
applyRightRules (Sequent gs ds) =
  let nonAtomicRight = filter (not . isAtomic) ds
  in concatMap (rightRuleFor (Sequent gs ds)) nonAtomicRight

rightRuleFor :: Sequent -> CPCSentence -> [(String, [Sequent])]
rightRuleFor (Sequent gs ds) f = case f of
  CPCAnd p q ->
    let ds' = delete f ds
        s1 = Sequent gs (p : ds')
        s2 = Sequent gs (q : ds')
    in [ ("And-Right", [s1, s2]) ]
  CPCOr p q ->
    let ds' = delete f ds
        s1 = Sequent gs (p : ds')
        s2 = Sequent gs (q : ds')
    in [ ("Or-Right-LeftChoice", [Sequent gs (p : ds')]) -- try proving with p
       , ("Or-Right-RightChoice", [Sequent gs (q : ds')]) -- or with q
       ]
  CPCImp p q ->
    -- To prove Γ ⊢ p → q, Δ  backward rule: add p to antecedent and try to prove Γ, p ⊢ q, Δ
    let ds' = delete f ds
        s1 = Sequent (p : gs) (q : ds')
    in [ ("Imp-Right", [s1]) ]
  CPCNot p ->
    -- ¬p on right: equivalent to proving p on left (Γ, p ⊢ )
    let ds' = delete f ds
        s1 = Sequent (p : gs) ds'
    in [ ("Not-Right", [s1]) ]
  _ -> []

  where ds = dsOf (Sequent gs ds)
        -- trivial accessor to silence shadowing warnings
        dsOf (Sequent _ d) = d

--------------------------------------------------------------------------------
-- LEFT rules (decompose formulas in antecedent; backward step)
--------------------------------------------------------------------------------

applyLeftRules :: Sequent -> [(String, [Sequent])]
applyLeftRules (Sequent gs ds) =
  let nonAtomicLeft = filter (not . isAtomic) gs
  in concatMap (leftRuleFor (Sequent gs ds)) nonAtomicLeft

leftRuleFor :: Sequent -> CPCSentence -> [(String, [Sequent])]
leftRuleFor (Sequent gs ds) f = case f of
  CPCAnd p q ->
    -- From Γ, (p∧q) ⊢ Δ we can derive Γ, p, q ⊢ Δ
    let gs' = delete f gs
        s1 = Sequent (p : q : gs') ds
    in [ ("And-Left", [s1]) ]
  CPCOr p q ->
    -- From Γ, (p∨q) ⊢ Δ we need both: Γ, p ⊢ Δ and Γ, q ⊢ Δ (since antecedent provides a case)
    let gs' = delete f gs
        s1 = Sequent (p : gs') ds
        s2 = Sequent (q : gs') ds
    in [ ("Or-Left", [s1, s2]) ]
  CPCImp p q ->
    -- From Γ, (p→q) ⊢ Δ we can try Γ ⊢ p, Δ  and Γ, q ⊢ Δ (right and left interplay).
    -- A common variant: (p→q) on left gives two premises:
    --   1) Γ ⊢ p, Δ
    --   2) Γ, q ⊢ Δ
    let gs' = delete f gs
        s1 = Sequent gs (p : ds)
        s2 = Sequent (q : gs') ds
    in [ ("Imp-Left", [s1, s2]) ]
  CPCNot p ->
    -- ¬p in antecedent: move p to succedent
    let gs' = delete f gs
        s1 = Sequent gs' (p : ds)
    in [ ("Not-Left", [s1]) ]
  _ -> []
  where ds = dsOf (Sequent gs ds)
        dsOf (Sequent _ d) = d

--------------------------------------------------------------------------------
-- Structural / weakening / contraction rules (simple heuristics)
-- These are used as last-resort steps to allow introducing or removing
-- formulas; in a full sequent calculus these are admissible or controlled.
--------------------------------------------------------------------------------

structuralRules :: Sequent -> [(String, [Sequent])]
structuralRules s@(Sequent gs ds) =
  -- Weakening: we may try removing a left or right formula (simulates search breadth)
  let leftWeak = case gs of
        (x:xs) -> [ ("Left-Weakening", [Sequent xs ds]) ]
        []     -> []
      rightWeak = case ds of
        (x:xs) -> [ ("Right-Weakening", [Sequent gs xs]) ]
        []     -> []
  in leftWeak ++ rightWeak

--------------------------------------------------------------------------------
-- Helpers: atomic test and pretty printing
--------------------------------------------------------------------------------

-- | Consider atomic if it is a CPCVar (an atom). Everything else is complex.
isAtomic :: CPCSentence -> Bool
isAtomic (CPCVar _) = True
isAtomic _          = False

-- Pretty-print helper for debugging and logging
prettySequent :: Sequent -> String
prettySequent (Sequent gs ds) =
  let gstr = commaSep (map show gs)
      dstr = commaSep (map show ds)
  in gstr ++ " ⊢ " ++ dstr
  where
    commaSep [] = "·"
    commaSep xs = foldr1 (\a b -> a ++ ", " ++ b) xs

-- Use show-based string key for visited set; could use a canonical form for stronger loop detection.
showSequent :: Sequent -> String
showSequent = prettySequent

--------------------------------------------------------------------------------
-- Note:
-- This implementation is intentionally compact and intended as a practical
-- starting point. A production-grade sequent prover would:
--  - include focused proof-search (e.g., invertible rules first),
--  - use canonical multisets/occurrence indices to avoid infinite churn,
--  - implement contraction/weakening/co-others under controlled strategies,
--  - support cut-elimination or incorporate analytic rules (G3 style),
--  - and generate richer diagnostics for failed proofs.
--
-- The current file produces an explicit ProofTree when a proof is found,
-- which can be serialized, audited, or pretty-printed and then passed to the
-- LLM for human-friendly explanations.
--------------------------------------------------------------------------------
