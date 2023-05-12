module QNKAT.PolicyEmbeddings where

import Data.List.NonEmpty (NonEmpty)

import QNKAT.Definitions.Core
import QNKAT.Definitions.Structures
import QNKAT.Definitions.Policy
import QNKAT.Utils.NonEmpty

class HasMeaning p a where
    meaning :: p -> a

instance Quantum a t => HasMeaning (TaggedAction t) a where
    meaning = tryCreateBellPairFrom . actionArgs

instance TestsOrderedQuantum a t => HasMeaning (NonEmpty (Atomic t)) a where
    meaning ta = liftLayer $ foldNonEmpty (<.>) $ meaning <$> ta

instance (ParallelSemigroup a, Semigroup a, HasMeaning at a) => HasMeaning (Policy at) a where
    meaning (APAtomic ta)    = meaning ta
    meaning (APSequence p q) = meaning p <> meaning q
    meaning (APParallel p q) = meaning p <||> meaning q

instance (TestsOrderedQuantum a t) => HasMeaning (Atomic t) (Layer a) where
    meaning (AAction a) = orderedTryCreateBellPairFrom $ actionArgs a
    meaning (ATest t)   = orderedTest t

instance (ChoiceSemigroup a, Monoid a, TestsOrderedQuantum a t) 
  => HasMeaning (Ordered FullPolicy t) a where
    meaning (FPAtomic ta) = liftLayer $ foldNonEmpty (<.>) $ meaning <$> ta
    meaning (FPSequence p q) = meaning p <> meaning q
    meaning FPOne = mempty
    meaning (FPParallel p q) = meaning p <||> meaning q
    meaning (FPChoice p q) = meaning p <+> meaning q

instance (MonoidStar a, TestsOrderedQuantum a t) => HasMeaning (Ordered StarPolicy t) a where
    meaning (SPAtomic ta) = liftLayer $ foldNonEmpty (<.>) $ meaning <$> ta
    meaning (SPSequence p q) = meaning p <> meaning q
    meaning SPOne = mempty
    meaning (SPStar p) = star (meaning p)
    meaning (SPParallel p q) = meaning p <||> meaning q
    meaning (SPChoice p q) = meaning p <+> meaning q

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
