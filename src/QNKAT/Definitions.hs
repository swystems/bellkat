module QNKAT.Definitions
    ( module QNKAT.Definitions.Core
    , module QNKAT.Definitions.Structures
    , module QNKAT.Definitions.Policy
    , meaning, applyPolicy, applyPolicyTimely, applyPolicySteps, applyOrderedPolicy, applyFullOrderedPolicy
    ) where

import           Data.Set                                (Set)

import           QNKAT.Definitions.Structures
import           QNKAT.Definitions.Core
import           QNKAT.Definitions.Policy
import           QNKAT.Utils.NonEmpty
import qualified QNKAT.Implementations.HistoryQuantum        as HQ
import qualified QNKAT.Implementations.OneStepHistoryQuantum as OSHQ
import qualified QNKAT.Implementations.StepHistoryQuantum    as SHQ
import qualified QNKAT.Implementations.TimelyHistoryQuantum  as THQ

-- | methods

actionArgs :: TaggedAction t -> CreateBellPairArgs t
actionArgs ta = case taAction ta of
    (Swap l (l1, l2))     -> CreateBellPairArgs (taTagPredicate ta)
        (l1 :~: l2) [l :~: l1, l :~: l2] Nothing (taTag ta) (taDup ta)
    (Transmit l (l1, l2)) -> CreateBellPairArgs (taTagPredicate ta)
        (l1 :~: l2) [l :~: l] Nothing (taTag ta) (taDup ta)
    (Create l)            -> CreateBellPairArgs (taTagPredicate ta)
        (l :~: l) [] Nothing (taTag ta) (taDup ta)
    (Distill (l1, l2))    -> CreateBellPairArgs (taTagPredicate ta)
        (l1 :~: l2) [l1 :~: l2, l1 :~: l2] (Just 0.5) (taTag ta) (taDup ta)

class HasMeaning p a where
    meaning :: p -> a

instance (ParallelSemigroup a, Quantum a t) => HasMeaning (Normal Policy t) a where
    meaning (APAtomic ta)    = tryCreateBellPairFrom $ actionArgs ta
    meaning (APSequence p q) = meaning p <> meaning q
    meaning (APParallel p q) = meaning p <||> meaning q

instance (TestsOrderedQuantum a t) => HasMeaning (Atomic t) (Layer a) where
    meaning (AAction a) = orderedTryCreateBellPairFrom $ actionArgs a
    meaning (ATest t)   = orderedTest t

instance (TestsOrderedQuantum a t) => HasMeaning (Ordered Policy t) a where
    meaning (APAtomic ta) = liftLayer $ foldNonEmpty (<.>) $ meaning <$> ta
    meaning (APSequence p q) = meaning p <> meaning q
    meaning (APParallel p q) = meaning p <||> meaning q

instance (ChoiceSemigroup a, Monoid a, TestsOrderedQuantum a t) 
  => HasMeaning (Ordered FullPolicy t) a where
    meaning (FPAtomic ta) = liftLayer $ foldNonEmpty (<.>) $ meaning <$> ta
    meaning (FPSequence p q) = meaning p <> meaning q
    meaning FPOne = mempty
    meaning (FPParallel p q) = meaning p <||> meaning q
    meaning (FPChoice p q) = meaning p <+> meaning q

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
