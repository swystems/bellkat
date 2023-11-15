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
import qualified BellKAT.Implementations.OneStepHistoryQuantum as OSHQ
import qualified BellKAT.Implementations.StepHistoryQuantum    as SHQ
import qualified BellKAT.Implementations.AutomataStepHistoryQuantum    as ASHQ
import qualified BellKAT.Implementations.TimelyHistoryQuantum  as THQ

applyPolicy :: Ord t => Normal Policy t -> History t -> Set (History t)
applyPolicy = HQ.execute . meaning

applyPolicyTimely :: Ord t => Normal Policy t -> History t -> Set (History t)
applyPolicyTimely = THQ.execute . meaning

applyPolicySteps :: (Ord t) => Normal Policy t -> History t -> Set (History t)
applyPolicySteps  = SHQ.execute HQ.execute . meaning

applyOrderedPolicy :: (Ord t, Show t) => Ordered Policy t -> History t -> Set (History t)
applyOrderedPolicy = SHQ.execute OSHQ.execute . meaning

applyFullOrderedPolicy :: (Ord t, Show t) => Ordered FullPolicy t -> History t -> Set (History t)
applyFullOrderedPolicy = SHQ.execute OSHQ.execute . meaning

applyFullOrderedPolicyAuto :: (Ord t, Show t) => Ordered FullPolicy t -> History t -> Set (History t)
applyFullOrderedPolicyAuto = ASHQ.execute OSHQ.execute . meaning

applyStarOrderedPolicy :: (Ord t, Show t) => Ordered StarPolicy t -> History t -> Set (History t)
applyStarOrderedPolicy = ASHQ.execute OSHQ.execute . meaning

applyOneStepPolicyPartial :: (Ord t, Show t) => Normal OneRoundPolicy t -> History t -> Set (Partial (History t))
applyOneStepPolicyPartial = OSHQ.executePartial . meaning

applyOneStepPolicy :: (Ord t, Show t) => Normal OneRoundPolicy t -> History t -> Set (History t)
applyOneStepPolicy = OSHQ.execute . meaning

applyStarOrderedPolicyBounded :: (Ord t, Show t) => Ordered StarPolicy t -> History t -> Set (History t)
applyStarOrderedPolicyBounded = (handleExecutionError .) . ASHQ.executeWith (ASHQ.EP (Just 100)) OSHQ.execute . meaning
  where
    handleExecutionError :: Maybe a -> a
    handleExecutionError Nothing = error "couldn't execute"
    handleExecutionError (Just x) = x
