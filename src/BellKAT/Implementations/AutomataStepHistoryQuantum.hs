{-# LANGUAGE StrictData #-}
module BellKAT.Implementations.AutomataStepHistoryQuantum 
    ( AutomatonStepHistoryQuantum (getNFA)
    , execute
    , executeWith
    , ExecutionParams(..)
    ) where

import           Data.Pointed
import           Data.Set                       (Set)
import           Data.Maybe                     (fromJust)
 

import           BellKAT.Definitions.Structures
import           BellKAT.Implementations.Automata
import qualified BellKAT.Implementations.AutomataExecution as AE
import           BellKAT.Implementations.AutomataExecution (ExecutionParams)

newtype AutomatonStepHistoryQuantum a = AutomatonStepHistoryQuantum 
    { getNFA :: MagicNFA a
    } deriving newtype (Show, ParallelSemigroup, OrderedSemigroup, Pointed, Semigroup, Monoid, ChoiceSemigroup, MonoidStar)

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

instance (Ord tag, ChoiceSemigroup (sq tag), TestsQuantum (sq tag) test tag)
        => TestsOrderedQuantum (AutomatonStepHistoryQuantum (sq tag)) test tag where
    orderedTest = OneStep . test

execute :: (Ord b, Show b)
    => (a -> b -> Set b)
    -> AutomatonStepHistoryQuantum a
    -> b -> Set b
execute executeStep ahq = fromJust . executeWith (AE.EP Nothing) executeStep ahq

executeWith :: (Ord b, Show b)
    => ExecutionParams
    -> (a -> b -> Set b)
    -> AutomatonStepHistoryQuantum a
    -> b -> Maybe (Set b)
executeWith params executeStep (AutomatonStepHistoryQuantum nfa) = AE.execute params executeStep nfa
