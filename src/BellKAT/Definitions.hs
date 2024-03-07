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
    , applyStarOrderedPolicyBounded
    , applyOneStepPolicy
    , applyOneStepPolicyPartial
    ) where

import           Data.Set                                (Set)

import           BellKAT.Definitions.Structures
import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Policy
import           BellKAT.PolicyEmbeddings
import qualified BellKAT.Implementations.HistoryQuantum        as HQ
import qualified BellKAT.Implementations.InterleavingOneStepHistoryQuantum as IOSHQ
import qualified BellKAT.Implementations.StepHistoryQuantum    as SHQ
import qualified BellKAT.Implementations.AutomataStepHistoryQuantum    as ASHQ
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
applyOrderedPolicy = SHQ.execute IOSHQ.execute . meaning

applyFullOrderedPolicy 
    :: (Ord tag, Show tag) 
    => Ordered FullPolicy BellPairsPredicate tag -> History tag -> Set (History tag)
applyFullOrderedPolicy = SHQ.execute IOSHQ.execute . meaning

applyFullOrderedPolicyAuto 
    :: (Ord tag, Show tag) 
    => Ordered FullPolicy BellPairsPredicate tag -> History tag -> Set (History tag)
applyFullOrderedPolicyAuto = ASHQ.execute IOSHQ.execute . meaning

applyStarOrderedPolicy 
    :: (Ord tag, Show tag) 
    => Ordered StarPolicy BellPairsPredicate tag -> History tag -> Set (History tag)
applyStarOrderedPolicy = ASHQ.execute IOSHQ.execute . meaning

applyOneStepPolicyPartial 
    :: (Ord tag, Show tag) 
    => Normal OneRoundPolicy tag -> History tag -> Set (Partial (History tag))
applyOneStepPolicyPartial = IOSHQ.executePartial . meaning

applyOneStepPolicy 
    :: (Ord tag, Show tag) 
    => Normal OneRoundPolicy tag -> History tag -> Set (History tag)
applyOneStepPolicy = IOSHQ.execute . meaning

applyStarOrderedPolicyBounded 
    :: (Ord tag, Show tag) 
    => Ordered StarPolicy BellPairsPredicate tag -> History tag -> Set (History tag)
applyStarOrderedPolicyBounded = (handleExecutionError .) . ASHQ.executeWith (ASHQ.EP (Just 100)) IOSHQ.execute . meaning
  where
    handleExecutionError :: Maybe a -> a
    handleExecutionError Nothing = error "couldn't execute"
    handleExecutionError (Just x) = x
