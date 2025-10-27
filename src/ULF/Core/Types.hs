{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : ULF.Core.Types
-- Description : Canonical, shared primitive types for the ULF v11 architecture.
-- Copyright   : (c) 2025, ULF Genesis Project
-- License     : MIT
-- Maintainer  : The Guardian
-- Stability   : experimental
--
-- The lingua franca of ULF: Atoms, Symbols, Signatures, Renamings (functional
-- and finite), and small utilities to validate and compose signature morphisms.
module ULF.Core.Types
  ( -- * Primitives
    Atom
  , Sentence

    -- * Symbols
  , Symbol(..)
  , symbolAtomCandidates

    -- * Renamings / Morphisms
  , Renaming
  , RenamingMap
  , renamingToFun
  , applyRenamingMap
  , composeRenamingMap

    -- * Signatures
  , Signature(..)
  , SignatureError(..)
  , signatureAtoms
  , validateRenaming

    -- * Convenience aliases
  , AtomSet
  , SymbolSet
  ) where

import GHC.Generics (Generic)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Basic primitives
-------------------------------------------------------------------------------

-- | The most primitive unit of meaning in ULF.
-- Chosen as 'String' for human-readability and ease of debugging.
type Atom = String

-- | Generic textual representation of a well-formed formula / sentence.
-- Institutions should define their own concrete Sentence types where needed.
type Sentence = String

-------------------------------------------------------------------------------
-- Symbols
-------------------------------------------------------------------------------

-- | A Symbol is a member of an institution's alphabet (Σ).
-- Distinguishes logical connectives from non-logical constants and variables.
data Symbol
  = LogicalCon Text       -- ^ logical token (e.g., "∧", "¬", "□")
  | NonLogicalConst Atom  -- ^ domain-level constant (an 'Atom')
  | Variable Text         -- ^ syntactic variable (e.g., "x", "y")
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | Extract candidate Atoms from a Symbol (empty list for logical or variable tokens).
symbolAtomCandidates :: Symbol -> [Atom]
symbolAtomCandidates = \case
  NonLogicalConst a -> [a]
  _                 -> []

-------------------------------------------------------------------------------
-- Renaming / Morphisms
-------------------------------------------------------------------------------

-- | A total renaming function from Atom -> Atom (executable form of a signature morphism).
type Renaming = Atom -> Atom

-- | Finite, inspectable representation of a renaming (serializable).
type RenamingMap = Map Atom Atom

-- | Convert a RenamingMap to a total Renaming function (falls back to identity).
renamingToFun :: RenamingMap -> Renaming
renamingToFun rm a = M.findWithDefault a a rm

-- | Apply a RenamingMap to a single Atom (identity if absent).
applyRenamingMap :: RenamingMap -> Atom -> Atom
applyRenamingMap = renamingToFun

-- | Compose two RenamingMaps (right-to-left): (r2 . r1)(a) = r2 (r1 a).
-- Atoms not present are treated as identity.
composeRenamingMap :: RenamingMap -> RenamingMap -> RenamingMap
composeRenamingMap r2 r1 =
  let allKeys = S.union (S.fromList $ M.keys r1) (S.fromList $ M.keys r2)
      composed k = applyRenamingMap r2 (applyRenamingMap r1 k)
  in M.fromList [ (k, composed k) | k <- S.toList allKeys ]

-------------------------------------------------------------------------------
-- Signatures
-------------------------------------------------------------------------------

-- | Parametric Signature type. 's' is the sentence/axiom type used by an institution.
data Signature s = Signature
  { symbols :: Set Symbol  -- ^ Alphabet for the signature
  , axioms  :: Set s       -- ^ Optional background axioms / theory (may be empty)
  } deriving (Eq, Show, Generic)

instance (ToJSON s) => ToJSON (Signature s)
instance (FromJSON s) => FromJSON (Signature s)

-- | Convenience type aliases.
type AtomSet   = Set Atom
type SymbolSet = Set Symbol

-- | Extract the non-logical Atoms declared in a Signature's alphabet.
signatureAtoms :: Signature s -> AtomSet
signatureAtoms sig =
  let sb = symbols sig
      atoms = foldr (\sym acc -> acc <> S.fromList (symbolAtomCandidates sym)) S.empty sb
  in atoms

-- | Errors that may arise during renaming/signature validation.
data SignatureError
  = SourceAtomMissing Atom  -- ^ a key in the renaming isn't in the source signature
  | TargetAtomMissing Atom  -- ^ renaming maps to an atom not present in the target signature
  | DuplicateMapping Atom   -- ^ duplicate mapping for same source (shouldn't happen in Map)
  | GenericSigError String
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Validate a finite renaming (RenamingMap) from source -> target Signature.
-- Ensures:
--   * Every source key exists in the source signature.
--   * Every target value exists in the target signature.
validateRenaming :: Signature s -> Signature t -> RenamingMap -> Either SignatureError ()
validateRenaming srcSig tgtSig rm =
  let srcAtoms = signatureAtoms srcSig
      tgtAtoms = signatureAtoms tgtSig
      checkKey k =
        if k `S.member` srcAtoms
          then Right ()
          else Left (SourceAtomMissing k)
      checkVal v =
        if v `S.member` tgtAtoms
          then Right ()
          else Left (TargetAtomMissing v)
      checks = map (\(k,v) -> checkKey k >> checkVal v) (M.toList rm)
  in case sequence checks of
       Left e  -> Left e
       Right _ -> Right ()

-------------------------------------------------------------------------------
-- Notes
-------------------------------------------------------------------------------
-- * 'Signature s' is intentionally parametric so each institution can keep its own sentence
--   representation (e.g., CPCSentence, ATLSentence) while still sharing the
--   signature vocabulary types.
-- * Consider later introducing an 'Atom' interning layer (e.g., newtype AtomId = AtomId Int)
--   if profiling shows string-heavy bottlenecks in large experiment traces.
-- * The module intentionally exposes both functional and finite renaming forms:
--   * use RenamingMap for serializable, inspectable morphisms stored in MAL/configs,
--   * use Renaming (function) for fast application/composition at runtime.
