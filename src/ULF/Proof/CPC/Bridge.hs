{-# LANGUAGE OverloadedStrings #-}

-- | ULF v11 Genesis
-- | Module: ULF.Proof.CPC.Bridge
-- | Purpose: Translation and interoperability between proof formalisms.
-- | Author: The ULF Genesis Collective
-- | License: MIT

module ULF.Proof.CPC.Bridge (
    sequentToNaturalDeduction,
    proofTreeToExplanation,
    proofTreeToText,
    crossInstitutionTranslate
) where

import ULF.Institutions.CPC (CPCSentence(..))
import ULF.Proof.CPC.Sequent (Sequent(..), ProofTree(..))
import qualified ULF.Proof.CPC.NaturalDeduction as ND
import qualified ULF.Institutions.LLM as LLM
import qualified ULF.Institutions.FDE as FDE
import Data.List (intercalate)

-- | Translates a sequent-style proof into a natural deduction proof.
-- | This allows human-level inspection and teaching use-cases.
sequentToNaturalDeduction :: ProofTree -> ND.NDProofTree
sequentToNaturalDeduction (Axiom s) =
    ND.Axiom (ND.fromSequent s)
sequentToNaturalDeduction (Inference rule conc premises) =
    ND.Inference
      { ND.ruleName   = "Translated-" ++ rule
      , ND.conclusion = ND.fromSequent conc
      , ND.premises   = map sequentToNaturalDeduction premises
      }
  where rule = ruleName rule

-- | Converts a formal proof tree into a plain-text, indented representation.
-- | This is the intermediate representation between logic and language.
proofTreeToText :: ProofTree -> String
proofTreeToText = go 0
  where
    go indent (Axiom s) =
      replicate indent ' ' ++ "Axiom: " ++ show s
    go indent (Inference name conc premises) =
      replicate indent ' ' ++ name ++ " ⊢ " ++ show conc ++ "\n"
      ++ concatMap (\p -> go (indent + 2) p ++ "\n") premises

-- | Generates a human-readable explanation using the LLM institution.
-- | This converts the formal structure into narrative reasoning.
proofTreeToExplanation :: ProofTree -> IO LLM.LLMSentence
proofTreeToExplanation tree = do
    let linear = proofTreeToText tree
        prompt = unlines
          [ "Explain the following logical derivation in clear natural language:"
          , linear
          , "Use simple, structured steps and intuitive explanations."
          ]
    LLM.generateExplanation prompt

-- | Translates a CPC proof into an equivalent structure for another institution.
-- | Enables cross-logic validation and reflective reasoning.
crossInstitutionTranslate :: ProofTree -> String -> Maybe FDE.FDEProof
crossInstitutionTranslate tree target =
    case target of
      "FDE" -> Just (FDE.fromCPCProof tree)
      "K"   -> Nothing  -- Placeholder for modal logic conversion
      _     -> Nothing
