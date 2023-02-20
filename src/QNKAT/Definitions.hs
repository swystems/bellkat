module QNKAT.Definitions 
    ( module QNKAT.Definitions.Core
    , module QNKAT.Definitions.Policy
    , meaning, applyPolicy, applyPolicyTimely, applyPolicySteps
    ) where

import           Data.Set                   (Set)

import           QNKAT.Definitions.Core
import           QNKAT.Definitions.Policy
import qualified QNKAT.Definitions.HistoryQuantum as HQ
import qualified QNKAT.Definitions.TimelyHistoryQuantum as THQ
import qualified QNKAT.Definitions.StepHistoryQuantum as SHQ

-- | methods
meaning :: Quantum a t => Policy t -> a
meaning (Atomic ta) = case taAction ta of
    (Swap l (l1, l2))     -> tryCreateBellPairFrom (taTagPredicate ta)
        (l1 :~: l2) [l :~: l1, l :~: l2] Nothing (taTag ta) (taDup ta)
    (Transmit l (l1, l2)) -> tryCreateBellPairFrom (taTagPredicate ta)
        (l1 :~: l2) [l :~: l] Nothing (taTag ta) (taDup ta)
    (Create l)            -> tryCreateBellPairFrom (taTagPredicate ta)
        (l :~: l) [] Nothing (taTag ta) (taDup ta)
    (Distill (l1, l2))    -> tryCreateBellPairFrom (taTagPredicate ta)
        (l1 :~: l2) [l1 :~: l2, l1 :~: l2] (Just 0.5) (taTag ta) (taDup ta)
meaning (Sequence p q)        = meaning p <> meaning q
meaning (Parallel p q)        = meaning p <||> meaning q

applyPolicy :: Ord t => Policy t -> History t -> Set (History t)
applyPolicy = HQ.execute . meaning

applyPolicyTimely :: Ord t => Policy t -> History t -> Set (History t)
applyPolicyTimely = THQ.execute . meaning

applyPolicySteps :: (Ord t) => Policy t -> History t -> Set (History t)
applyPolicySteps p h = SHQ.execute h (meaning p)
