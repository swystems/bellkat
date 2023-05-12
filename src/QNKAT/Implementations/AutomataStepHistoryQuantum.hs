{-# LANGUAGE StrictData #-}
module QNKAT.Implementations.AutomataStepHistoryQuantum 
    ( AutomatonStepHistoryQuantum
    , execute
    ) where

import           Data.Pointed
import           Data.Set                       (Set)
 

import           QNKAT.Definitions.Core
import           QNKAT.Definitions.Structures
import           QNKAT.Implementations.Automata
import qualified QNKAT.Implementations.AutomataExecution as AE

newtype AutomatonStepHistoryQuantum a = AutomatonStepHistoryQuantum (MagicNFA a)
    deriving newtype (Show, ParallelSemigroup, Pointed, Semigroup, Monoid, ChoiceSemigroup, MonoidStar)

instance (Ord t, ChoiceSemigroup (sq t), CreatesBellPairs (sq t) t)
        => CreatesBellPairs (AutomatonStepHistoryQuantum (sq t)) t where
    tryCreateBellPairFrom = point . tryCreateBellPairFrom

instance (Ord t, ChoiceSemigroup (sq t), Quantum (sq t) t)
        => Quantum (AutomatonStepHistoryQuantum (sq t)) t where

instance (ChoiceSemigroup (sq t), Quantum (sq t) t) 
        => OrderedQuantum (AutomatonStepHistoryQuantum (sq t)) t where
    newtype Layer (AutomatonStepHistoryQuantum (sq t)) = OneStep (sq t)
    orderedTryCreateBellPairFrom = OneStep . tryCreateBellPairFrom
    liftLayer (OneStep s) = point s

instance (Semigroup (sq t)) => OrderedSemigroup (Layer (AutomatonStepHistoryQuantum (sq t))) where
   (OneStep s) <.> (OneStep s') = OneStep (s <> s')

instance (Ord t, ChoiceSemigroup (sq t), TestsQuantum (sq t) t) 
        => TestsOrderedQuantum (AutomatonStepHistoryQuantum (sq t)) t where
    orderedTest = OneStep . test

execute :: Ord t
    => (a -> History t -> Set (History t))
    -> AutomatonStepHistoryQuantum a
    -> History t -> Set (History t)
execute executeStep (AutomatonStepHistoryQuantum nfa) = AE.execute executeStep nfa
