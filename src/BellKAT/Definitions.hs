{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module BellKAT.Definitions
    ( module BellKAT.Definitions.Core
    , module BellKAT.Definitions.Structures
    , module BellKAT.Definitions.Policy
    , applyPolicy
    , applyPolicyTimely
    , applyPolicySteps
    , applyOrderedPolicy
    , applyFullOrderedPolicy
    , applyFullOrderedPolicyAuto
    , applyStarOrderedPolicy
    , applyStarPolicyWithValidity
    , applyStarOrderedPolicyBounded
    , applyOneStepPolicy
    , applyOneStepPolicyPartial
    , applyStarPolicy
    , applyStarPolicyH
    ) where

import           Data.Set                                (Set)
import           Data.Default

import           BellKAT.Definitions.Structures
import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Policy
import           BellKAT.PolicyEmbeddings
import qualified BellKAT.Implementations.HistoryQuantum        as HQ
import qualified BellKAT.Implementations.InterleavingOneStepHistoryQuantum as IOSHQ
import qualified BellKAT.Implementations.StepHistoryQuantum    as SHQ
import qualified BellKAT.Implementations.AutomataStepHistoryQuantum    as ASHQ
import qualified BellKAT.Implementations.AtomicOneStepHistoryQuantum  as AOSHQ
import qualified BellKAT.Implementations.TimelyHistoryQuantum  as THQ

applyPolicy :: Ord tag => Normal Policy tag -> History tag -> Set (History tag)
applyPolicy = HQ.execute . meaning

applyPolicyTimely :: Ord tag => Normal Policy tag -> History tag -> Set (History tag)
applyPolicyTimely = THQ.execute . meaning

applyPolicySteps :: (Ord tag) => Normal Policy tag -> History tag -> Set (History tag)
applyPolicySteps  = SHQ.execute HQ.execute . meaning

applyOrderedPolicy 
    :: (Ord tag, Show tag) 
    => Ordered Policy BellPairsPredicate tag -> History tag -> Set (History tag)
applyOrderedPolicy = SHQ.execute (IOSHQ.execute @'IOSHQ.FDUse) . meaning

applyFullOrderedPolicy 
    :: (Ord tag, Show tag) 
    => Ordered FullPolicy BellPairsPredicate tag -> History tag -> Set (History tag)
applyFullOrderedPolicy = SHQ.execute (IOSHQ.execute @'IOSHQ.FDUse) . meaning

applyFullOrderedPolicyAuto 
    :: (Ord tag, Show tag) 
    => Ordered FullPolicy BellPairsPredicate tag -> History tag -> Set (History tag)
applyFullOrderedPolicyAuto = ASHQ.executeE (IOSHQ.execute @'IOSHQ.FDUse) . meaning

applyStarOrderedPolicy 
    :: (Ord tag, Show tag) 
    => Ordered StarPolicy BellPairsPredicate tag -> History tag -> Set (History tag)
applyStarOrderedPolicy = ASHQ.executeE (IOSHQ.execute @'IOSHQ.FDUse) . meaning

applyStarPolicy 
    :: (Ord tag, Show tag, Default tag, Tests (AOSHQ.AtomicOneStepPolicy tag) test tag) 
    => NormalWithTests StarPolicy test tag -> TaggedBellPairs tag -> Set (TaggedBellPairs tag)
applyStarPolicy = ASHQ.execute AOSHQ.execute . meaning 

applyStarPolicyH
    :: (Ord tag, Show tag, Tests (IOSHQ.FunctionStep 'IOSHQ.FDTimely test tag) test tag) 
    => NormalWithTests StarPolicy test tag -> History tag -> Set (History tag)
applyStarPolicyH = ASHQ.executeE (IOSHQ.execute @'IOSHQ.FDTimely) . meaning

applyStarPolicyWithValidity
    :: (Ord tag, Show tag, Default tag, Tests (AOSHQ.AtomicOneStepPolicy tag) test tag) 
    => (TaggedBellPairs tag -> Bool)
    -> NormalWithTests StarPolicy test tag 
    -> TaggedBellPairs tag 
    -> Maybe (Set (TaggedBellPairs tag))
applyStarPolicyWithValidity isValid = 
    ASHQ.executeWith (def { ASHQ.isValidState = isValid }) AOSHQ.execute . meaning 

applyOneStepPolicyPartial 
    :: (Ord tag, Show tag) 
    => Normal OneRoundPolicy tag -> History tag -> Set (Partial (History tag))
applyOneStepPolicyPartial = (IOSHQ.executePartial @'IOSHQ.FDUse) . meaning

applyOneStepPolicy 
    :: (Ord tag, Show tag) 
    => Normal OneRoundPolicy tag -> History tag -> Set (History tag)
applyOneStepPolicy = (IOSHQ.execute @'IOSHQ.FDUse) . meaning

applyStarOrderedPolicyBounded 
    :: (Ord tag, Show tag) 
    => Ordered StarPolicy BellPairsPredicate tag -> History tag -> Set (History tag)
applyStarOrderedPolicyBounded = 
    (handleExecutionError .) 
    . ASHQ.executeWithE (def { ASHQ.maxOptionsPerState = Just 100}) (IOSHQ.execute @'IOSHQ.FDUse)
    . meaning
  where
    handleExecutionError :: Maybe a -> a
    handleExecutionError Nothing = error "couldn't execute"
    handleExecutionError (Just x) = x
