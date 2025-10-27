{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ULF.Institutions.CPC
-- Description : Classical Propositional Calculus — The Foundation of Certainty
-- Copyright   : (c) ULF Genesis Project
-- License     : MIT
--
-- The CPC module implements the simplest and most certain of all logical systems:
-- Classical Propositional Calculus. It defines the formal syntax, semantics, and
-- satisfaction relation for truth-functional reasoning. In the ULF architecture,
-- CPC serves as the ultimate arbiter of deductive consistency and truth.

module ULF.Institutions.CPC
  ( -- * Syntax
    CPCSentence(..),
    -- * Semantics
    CPCModel,
    satisfiesCPC,
    -- * Institution Instance
    CPCI(..)
  ) where

import           Data.Aeson   (ToJSON, FromJSON)
import           GHC.Generics (Generic)
import           ULF.Core.Types
import           ULF.Core.Institution

--------------------------------------------------------------------------------
-- 1. Syntax: The Formal Grammar of Propositional Logic
--------------------------------------------------------------------------------

-- | Represents a well-formed formula (WFF) in Classical Propositional Calculus.
-- Each constructor corresponds to a logical connective in the language.
data CPCSentence
  = CPCVar Atom                      -- ^ Atomic proposition (e.g., "p")
  | CPCNot CPCSentence               -- ^ Negation (¬p)
  | CPCAnd CPCSentence CPCSentence   -- ^ Conjunction (p ∧ q)
  | CPCOr  CPCSentence CPCSentence   -- ^ Disjunction (p ∨ q)
  | CPCImp CPCSentence CPCSentence   -- ^ Implication (p → q)
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- 2. Semantics: The World of Truth Assignments
--------------------------------------------------------------------------------

-- | A model (or possible world) for CPC is a valuation function that assigns
-- a Boolean truth value to each atomic proposition.
type CPCModel = Atom -> Bool

--------------------------------------------------------------------------------
-- 3. Satisfaction Relation: Truth Evaluation
--------------------------------------------------------------------------------

-- | The satisfaction relation (⊨) for CPC. This function recursively evaluates
-- a well-formed formula under a given model.
satisfiesCPC :: CPCModel -> CPCSentence -> Bool
satisfiesCPC model (CPCVar atom)    = model atom
satisfiesCPC model (CPCNot p)       = not (satisfiesCPC model p)
satisfiesCPC model (CPCAnd p q)     = satisfiesCPC model p && satisfiesCPC model q
satisfiesCPC model (CPCOr  p q)     = satisfiesCPC model p || satisfiesCPC model q
satisfiesCPC model (CPCImp p q)     = not (satisfiesCPC model p) || satisfiesCPC model q

--------------------------------------------------------------------------------
-- 4. The Institution: Connecting Syntax and Semantics
--------------------------------------------------------------------------------

-- | The Institution for Classical Propositional Calculus.
-- It defines the link between syntax (CPCSentence) and semantics (CPCModel)
-- via the satisfaction relation.
data CPCI = CPCI deriving (Eq, Show)

instance Institution CPCI where
  type Sentence CPCI = CPCSentence
  type Model    CPCI = CPCModel
  satisfies     = satisfiesCPC

--------------------------------------------------------------------------------
-- 5. Commentary: The Foundation of Logical Certainty
--------------------------------------------------------------------------------
-- The CPC Institution is the bedrock of deductive reasoning within ULF v11.
--
--  • Bivalence: Every sentence is either True or False.
--  • Truth-Functionality: The truth of a compound depends only on its parts.
--  • Monotonicity: Adding information never invalidates prior truths.
--
-- Within the ULF architecture:
--  • The SEI uses CPC as its "System 2" logical verifier.
--  • The Proof subsystem (ULF.Proof.CPC.Sequent) implements sequent calculus
--    and resolution techniques atop this core semantics.
--  • Other logics (ATL, FDE, etc.) ultimately anchor their consistency
--    to CPC's well-founded notion of truth.
--
-- In philosophical terms:
--  CPC is the still point of logical reality —
--  the immovable reference against which all uncertainty is measured.

--------------------------------------------------------------------------------
-- End of File
--------------------------------------------------------------------------------
