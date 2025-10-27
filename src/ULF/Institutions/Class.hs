{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : ULF.Institutions.Class
-- Description : The constitutional 'Institution' typeclass for ULF v11.
-- Copyright   : (c) 2025, ULF Genesis Project
-- License     : MIT
--
-- This module defines the universal interface that every logical "institution"
-- in the ULF architecture must implement.  It is the minimal, type-safe
-- contract expressing "what it means to be a logic" inside ULF.
--
-- The comments below annotate each definition with its formal meaning and
-- its philosophical/architectural role within the overall system.
module ULF.Institutions.Class
  ( -- * The Institution interface
    Institution(..)

    -- * Support types (re-exported for convenience)
  , module ULF.Core.Types     -- Atom, Renaming, Signature, etc.
  ) where

-- We rely on the project's canonical core types (Atom, Renaming, Signature).
-- These provide the basic vocabulary and lightweight infrastructure needed
-- for sentence translation ("mapSen") and other morphism-related utilities.
import ULF.Core.Types (Atom, Renaming, Signature)  -- assumed to exist in the project

--------------------------------------------------------------------------------
--  The Institution typeclass
--------------------------------------------------------------------------------

{-|
  The `Institution` typeclass is the formal, typed encoding of the key
  ingredients of institution theory (Goguen & Burstall):

    * A category of signatures (vocabularies) — encoded here externally as `Signature`
    * For each signature:
        - a set (type) of sentences (syntactic expressions)
        - a class (type) of models (semantic interpretations)
    * A satisfaction relation between models and sentences
    * Translation (functorial) operations that allow sentences and models to be
      transported along signature morphisms (here: renamings)

  In Haskell we express these attachments to an institution `i` using
  associated type families. This yields compile-time enforcement of
  interoperability: different institutions must declare what their
  sentences/models are, and the compiler ensures only compatible uses.
-}
class Institution i where

  {-|
  The type of well-formed formulas for this institution.

  Semantically: this represents the 'language' or 'syntax' of the logic.
  For CPC this would be a recursive AST type like `CPCSentence`.
  For ATL it would be `ATLSentence`, etc.

  Architecturally: every institution exposes its Sentence type so the SEI
  and other modules can construct queries, transform formulas, and
  serialize/deserialize as needed.
  -}
  type Sentence i :: *

  {-|
  The type of models (semantic structures) for this institution.

  Semantically: a model gives interpretation to the atoms and non-logical
  symbols of a sentence. Example: `Atom -> Bool` for CPC; `KModel w` for
  modal logics.

  Architecturally: models are the "worlds" or "possible states" the SEI uses
  to evaluate plans, check consistency, and run hypothetical simulations.
  -}
  type Model i :: *

  {-|
  The satisfaction relation (⊨).

  Semantically: `satisfies m φ` is true iff sentence `φ` holds in model `m`.

  This is the *bridge* between syntax and semantics. It is the invariant
  that makes logic into a science rather than arbitrary symbol manipulation.

  Architecturally/Practically:
    - The SEI calls `satisfies` to validate plans or to test hypotheses.
    - Proof modules will use `satisfies` when connecting syntactic proofs to
      semantic claims (e.g., model checking).
  -}
  satisfies :: Model i -> Sentence i -> Bool

  {-|
  Translate (rename) a sentence according to a `Renaming` (signature morphism).

  Semantically: this corresponds to the `Sen` functor in institution theory.
  Given a renaming σ : Σ → Σ' we produce a sentence in the target signature.

  Architecturally:
    - Enables the SEI to compare sentences across differing vocabularies.
    - Essential for grounding and symbol-mapping when connecting LLM outputs
      to formal institutions (LLM->CPC, LLM->ATL, etc).
  -}
  mapSen :: Renaming -> Sentence i -> Sentence i

  {-|
  (Optional/Advanced) Model reindex / reduct along a signature morphism.

  In full institution theory the `Mod` functor is contravariant: given a model
  in the target signature, we obtain a reduct in the source signature.

  Many practical ULF modules implement model translation at a higher layer
  (e.g., a centralized adapter) rather than via this method. We leave it
  as an optional method here (commented out) to avoid over-constraining
  early prototypes.

  If desired, uncomment and implement:
    reindex :: Renaming -> Model j -> Model i
  -}
  -- reindex :: Renaming -> Model j -> Model i

--------------------------------------------------------------------------------
--  Example-instance skeleton (commented) - shows how a concrete logic should
--  implement the Institution interface. Left as a commented example so that
--  the file remains purely declarative / constitutional.
--------------------------------------------------------------------------------

{-|
  Example (illustrative) — how CPC would implement the interface.

  Note: This block is explanatory and intentionally commented out so this file
  remains a pure "constitution" rather than depend on any particular institution.
  Remove comments and import the real CPC module to turn this into an actual
  instance.

  ----------------------------------------------------------------------------
  -- Example:
  data CPCI = CPCI

  instance Institution CPCI where
    type Sentence CPCI = CPCSentence   -- imported from ULF.Institutions.CPC
    type Model    CPCI = CPCModel      -- Atom -> Bool
    satisfies = satisfiesCPC          -- evaluate formula in model
    mapSen ren (CPCVar a) = CPCVar (ren a)
    mapSen ren (CPCNot p) = CPCNot (mapSen ren p)
    ... -- recursive cases for all constructors
  ----------------------------------------------------------------------------
-}

--------------------------------------------------------------------------------
--  Philosophical / architectural notes (embedded as comments):
--------------------------------------------------------------------------------

--  * Minimalism and universality:
--    The typeclass is intentionally minimal: only the essential operations
--    (sentence type, model type, satisfaction, sentence translation) are
--    required. This minimalist contract is what enables extreme pluralism:
--    new logics of any flavor can be added as long as they implement this
--    small interface.
--
--  * Type safety:
--    The use of associated type families ties sentences and models to the
--    institution identifier `i`. This ensures at compile-time that we never
--    accidentally apply a CPC model to an ATL sentence, or vice versa.
--
--  * Law-like invariants (informal):
--    Institution theory prescribes certain coherence properties that
--    implementations *should* maintain (informally):
--      - Satisfaction condition: translating a sentence and then evaluating
--        in the target model must correspond to evaluating in the original
--        model after appropriate model reindexing.
--    When full model reindexing is implemented, ULF modules should ensure
--    these properties hold; they form the "constitutional law" of the system.
--
--  * Role in the larger architecture:
--    - The SEI uses these interfaces to treat heterogeneous reasoning modules
--      uniformly (proof search, model checking, translation, etc.).
--    - The LLM institution can produce `Renaming` maps that the SEI applies
--      via `mapSen` to translate human text into the formal vocabulary.
--    - Proof-bridges use `mapSen` and `satisfies` to test whether parts of
--      proofs in one logic remain valid in another.
--
--  * Evolution and extension:
--    Because this file is the "constitution," changes to it are high-impact.
--    Additions are allowed (e.g., default methods, utility laws) but should be
--    done carefully and with a clear migration path for all `Institution`
--    instances already present in the codebase.
--
--------------------------------------------------------------------------------

-- End of file
