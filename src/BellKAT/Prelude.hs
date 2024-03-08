module BellKAT.Prelude (
    module BellKAT.DSL,
    module BellKAT.Definitions.Structures,
    BellKATPolicy,
    drawHistory,
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
import BellKAT.Drawing
import BellKAT.Test

type BellKATTag = (Maybe ())
type BellKATPolicy = NormalWithTests StarPolicy FreeTest BellKATTag

drawHistory :: BellKATPolicy -> IO ()
drawHistory = mainWith . drawStarPolicySteps

isPolicyValid ::
    Set (TaggedBellPairs BellKATTag) -> (TaggedBellPairs BellKATTag -> Bool) -> BellKATPolicy -> Bool
isPolicyValid initialStates isStateValid p =
    all (isJust . applyStarPolicyWithValidity isStateValid p) initialStates

arePoliciesEquivalentOn ::
    TaggedBellPairs BellKATTag -> BellKATPolicy -> BellKATPolicy -> Bool
arePoliciesEquivalentOn initialState = (==) `on` (`applyStarPolicy` initialState)

arePoliciesEquivalent ::
    Set (TaggedBellPairs BellKATTag) -> BellKATPolicy -> BellKATPolicy -> Bool
arePoliciesEquivalent initialStates p q =
    all (\is -> arePoliciesEquivalentOn is p q) initialStates
