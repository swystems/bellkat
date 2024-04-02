-- | This module is the entry point for working with @BellKAT@. The core exported type is that of
-- a @BellKAT@ policy, namely,'BellKATPolicy'. 'BellKAT should always be used to annotate the 
-- @BellKAT@ policy type to avoid ambiguity erros like in the example below:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE OverloadedLists   #-}
-- p :: 'BellKATPolicy'
-- p = 'create' \"C\"
-- @
-- Note, the two language extensions, 'OverloadedStrings' and 'OverloadedLists' should be enabled 
-- for ease of writing and checking policies.
--
module BellKAT.Prelude (
    BellKATPolicy,
    drawHistoriesSVG,
    drawHistoriesText,
    isPolicyValid,
    memoryBounds,
    arePoliciesEquivalent,
    -- * Re-exports from 'BellKAT.DSL'
    module BellKAT.DSL,
    -- * Re-exports from 'BellKAT.Definitions.Structures'
    module BellKAT.Definitions.Structures,
) where

import Data.Function
import Data.Maybe (isJust)
import Data.Set (Set)
import Diagrams.Backend.Cairo.CmdLine

import BellKAT.DSL
import BellKAT.Definitions
import BellKAT.Definitions.Structures hiding (test, (<.>))
import BellKAT.Drawing hiding (drawHistoriesText)
import BellKAT.Test

type BellKATTag = (Maybe ())
type BellKATPolicy = NormalWithTests StarPolicy FreeTest BellKATTag


drawHistoriesSVG :: BellKATPolicy -> IO ()
drawHistoriesSVG = mainWith . drawStarPolicySteps

drawHistoriesText :: BellKATPolicy -> IO ()
drawHistoriesText = putStrLn . drawStarPolicyStepsText

-- | Checks if a policy is valid w.r.t., to given valid states (N) and initial states (N_0).
-- 
-- Assuming a very simple policy defines as
--
-- @
-- p :: 'BellKATPolicy'
-- p = 'create' \"A\"
-- @
--
-- We can use 'memoryBounds' with the only initial state being empty set of Bell pairs to check that:
--
-- * one qubit at \"A\" is /not/ enough (we must hold the two qubits after 'create')
--
--      >>> isPolicyValid [[]] (memoryBounds [("A", 1)]) p
--      False
--
-- * two qubit at \"A\" are enough
--
--      >>> isPolicyValid [[]] (memoryBounds [("A", 2)]) p
--      True
--
-- We may also assume that there is already a Bell pair @\"A\" ~ \"B\"@ in the initial state, in
-- which case the last test will /not/ hold:
--
-- >>> isPolicyValid [[("A" ~ "B")]] (memoryBounds [("A", 2)]) p
-- False
isPolicyValid 
    :: Set (TaggedBellPairs BellKATTag) -- ^ `Set` of initial states
    -> (TaggedBellPairs BellKATTag -> Bool) -- ^ Predicate returning `True` on valid states
    -> BellKATPolicy -- ^ policy `p`
    -> Bool
isPolicyValid initialStates isStateValid p =
    all (isJust . applyStarPolicyWithValidity isStateValid p) initialStates

arePoliciesEquivalentOn ::
    TaggedBellPairs BellKATTag -> BellKATPolicy -> BellKATPolicy -> Bool
arePoliciesEquivalentOn initialState = (==) `on` (`applyStarPolicy` initialState)

-- | Checks if two policies are equivalent given a set of initial states (multisets of Bell pairs)
--
-- Most common example is for the initial states to include only the empty state (assuming
-- @OverloadedLists@ extension.
--
-- For instance, let's define two policies
--
-- @
-- p :: 'BellKATPolicy'
-- p = 'create' \"A\"
--
-- q :: 'BellKATPolicy'
-- q = 'create' \"B\" '<>' ('trans' \"B\" (\"A\", \"A\") '<||>' 'trans' \"A\" (\"B\", \"B\"))
-- @
-- 
-- We should have them behave the same on empty initial state
--
-- >>> arePoliciesEquivalent [[]] p q
-- True
--
-- But when we run on them again, i.e., starting from a single A~A pair, @q@ acts differently
--
-- >>> arePoliciesEquivalent [["A" ~ "A"]] p q
-- False
arePoliciesEquivalent 
    :: Set (TaggedBellPairs BellKATTag) -- ^ `Set` of initial states (N_0)
    -> BellKATPolicy -- ^ LHS policy `p`
    -> BellKATPolicy -- ^ RHS policy `q`
    -> Bool
arePoliciesEquivalent initialStates p q =
    all (\is -> arePoliciesEquivalentOn is p q) initialStates
