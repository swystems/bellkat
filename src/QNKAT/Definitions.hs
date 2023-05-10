module QNKAT.Definitions
    ( module QNKAT.Definitions.Core
    , module QNKAT.Definitions.Structures
    , module QNKAT.Definitions.Policy
    , applyPolicy, applyPolicyTimely, applyPolicySteps, applyOrderedPolicy, applyFullOrderedPolicy
    ) where

import           Data.Set                                (Set)

import           QNKAT.Definitions.Structures
import           QNKAT.Definitions.Core
import           QNKAT.Definitions.Policy
import           QNKAT.PolicyEmbeddings
import qualified QNKAT.Implementations.HistoryQuantum        as HQ
import qualified QNKAT.Implementations.OneStepHistoryQuantum as OSHQ
import qualified QNKAT.Implementations.StepHistoryQuantum    as SHQ
import qualified QNKAT.Implementations.TimelyHistoryQuantum  as THQ

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
