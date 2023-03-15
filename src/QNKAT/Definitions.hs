module QNKAT.Definitions
    ( module QNKAT.Definitions.Core
    , module QNKAT.Definitions.Policy
    , meaning, applyPolicy, applyPolicyTimely, applyPolicySteps, applyOrderedPolicy
    ) where

import           Data.Set                                (Set)

import           Data.List.NonEmpty                      (NonEmpty)
import           GHC.Base                                (NonEmpty (..))
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


meaning :: Quantum a t => Normal Policy t -> a
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

applyPolicy :: Ord t => Normal Policy t -> History t -> Set (History t)
applyPolicy = HQ.execute . meaning

applyPolicyTimely :: Ord t => Normal Policy t -> History t -> Set (History t)
applyPolicyTimely = THQ.execute . meaning

applyPolicySteps :: (Ord t) => Normal Policy t -> History t -> Set (History t)
applyPolicySteps  = SHQ.execute HQ.execute . meaning

applyOrderedPolicy :: (Ord t, Show t) => Ordered Policy t -> History t -> Set (History t)
applyOrderedPolicy = SHQ.execute OSHQ.execute . meaningOrdered

foldNonEmpty :: (a -> a -> a) -> NonEmpty a -> a
foldNonEmpty _ (x :| [])        = x
foldNonEmpty f (x :| (x' : xs)) = let y = f x x' in seq y (foldNonEmpty f (y :| xs))
