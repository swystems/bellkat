module BellKAT.Prelude (
    module BellKAT.DSL,
    module BellKAT.Definitions.Structures,
    BellKATPolicy,
    drawHistoriesSVG,
    drawHistoriesText,
    isPolicyValid,
    memoryBounds,
    arePoliciesEquivalent,
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

-- | Checks if a policy is valid w.r.t., to given valid states (N) and initial states (N_0)
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
arePoliciesEquivalent 
    :: Set (TaggedBellPairs BellKATTag) -- ^ `Set` of initial states (N_0)
    -> BellKATPolicy -- ^ LHS policy `p`
    -> BellKATPolicy -- ^ RHS policy `q`
    -> Bool
arePoliciesEquivalent initialStates p q =
    all (\is -> arePoliciesEquivalentOn is p q) initialStates
