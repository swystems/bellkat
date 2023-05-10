module QNKAT.Definitions
    ( module QNKAT.Definitions.Core
    , module QNKAT.Definitions.Structures
    , module QNKAT.Definitions.Policy
    , meaning, applyPolicy, applyPolicyTimely, applyPolicySteps, applyOrderedPolicy, applyFullOrderedPolicy
    ) where

import           Data.List.NonEmpty                      (NonEmpty)
import           Data.Set                                (Set)
import           GHC.Base                                (NonEmpty (..))

import           QNKAT.Definitions.Structures
import           QNKAT.Definitions.Core
import qualified QNKAT.Definitions.HistoryQuantum        as HQ
import qualified QNKAT.Definitions.OneStepHistoryQuantum as OSHQ
import           QNKAT.Definitions.Policy
import qualified QNKAT.Definitions.StepHistoryQuantum    as SHQ
import qualified QNKAT.Definitions.TimelyHistoryQuantum  as THQ

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

meaning :: (ParallelSemigroup a, Quantum a t) => Normal Policy t -> a
meaning (APAtomic ta)    = tryCreateBellPairFrom $ actionArgs ta
meaning (APSequence p q) = meaning p <> meaning q
meaning (APParallel p q) = meaning p <||> meaning q

meaningLayer :: (TestsOrderedQuantum a t) => Atomic t -> Layer a
meaningLayer (AAction a) = orderedTryCreateBellPairFrom $ actionArgs a
meaningLayer (ATest t)   = orderedTest t

meaningOrdered :: (TestsOrderedQuantum a t) => Ordered Policy t -> a
meaningOrdered (APAtomic ta) = fromLayer $ foldNonEmpty (<.>) $ meaningLayer <$> ta
meaningOrdered (APSequence p q) = meaningOrdered p <> meaningOrdered q
meaningOrdered (APParallel p q) = meaningOrdered p <||> meaningOrdered q

meaningOrderedFull
    :: (ChoiceSemigroup a, Monoid a, TestsOrderedQuantum a t) => Ordered FullPolicy t -> a
meaningOrderedFull (FPAtomic ta) = fromLayer $ foldNonEmpty (<.>) $ meaningLayer <$> ta
meaningOrderedFull (FPSequence p q) = meaningOrderedFull p <> meaningOrderedFull q
meaningOrderedFull FPOne = mempty
meaningOrderedFull (FPParallel p q) = meaningOrderedFull p <||> meaningOrderedFull q
meaningOrderedFull (FPChoice p q) = meaningOrderedFull p <+> meaningOrderedFull q

applyPolicy :: Ord t => Normal Policy t -> History t -> Set (History t)
applyPolicy = HQ.execute . meaning

applyPolicyTimely :: Ord t => Normal Policy t -> History t -> Set (History t)
applyPolicyTimely = THQ.execute . meaning

applyPolicySteps :: (Ord t) => Normal Policy t -> History t -> Set (History t)
applyPolicySteps  = SHQ.execute HQ.execute . meaning

applyOrderedPolicy :: (Ord t, Show t) => Ordered Policy t -> History t -> Set (History t)
applyOrderedPolicy = SHQ.execute OSHQ.execute . meaningOrdered

applyFullOrderedPolicy :: (Ord t, Show t) => Ordered FullPolicy t -> History t -> Set (History t)
applyFullOrderedPolicy = SHQ.execute OSHQ.execute . meaningOrderedFull

foldNonEmpty :: (a -> a -> a) -> NonEmpty a -> a
foldNonEmpty _ (x :| [])        = x
foldNonEmpty f (x :| (x' : xs)) = let y = f x x' in seq y (foldNonEmpty f (y :| xs))
