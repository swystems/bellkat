{-# LANGUAGE StrictData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module BellKAT.Implementations.AutomataStepHistoryQuantum 
    ( AutomatonStepHistoryQuantum (getNFA)
    , executeE
    , executeWithE
    , ExecutionParams(..)
    ) where

import           Data.Pointed
import           Data.Set                       (Set)
import           Data.Maybe                     (fromJust)
 

import           BellKAT.Definitions.Structures
import           BellKAT.Implementations.Automata
import qualified BellKAT.Implementations.AutomataExecution as AE
import           BellKAT.Implementations.AutomataExecution (ExecutionParams)

data AutomatonChoice = ACNormal | ACEmbedded

type family AutomatonFromChoice (ac :: AutomatonChoice) where
    AutomatonFromChoice 'ACNormal = HyperMagicNFA
    AutomatonFromChoice 'ACEmbedded = MagicNFA

newtype AutomatonStepHistoryQuantum (ac :: AutomatonChoice) a = AutomatonStepHistoryQuantum 
    { getNFA :: AutomatonFromChoice ac a
    }

deriving newtype instance Show (AutomatonFromChoice ac a) 
  => Show (AutomatonStepHistoryQuantum ac a)
deriving newtype instance ParallelSemigroup (AutomatonFromChoice ac a) 
  => ParallelSemigroup (AutomatonStepHistoryQuantum ac a)
deriving newtype instance OrderedSemigroup (AutomatonFromChoice ac a) 
  => OrderedSemigroup (AutomatonStepHistoryQuantum ac a)
deriving newtype instance Pointed (AutomatonFromChoice ac) 
  => Pointed (AutomatonStepHistoryQuantum ac)
deriving newtype instance Semigroup (AutomatonFromChoice ac a) 
  => Semigroup (AutomatonStepHistoryQuantum ac a)
deriving newtype instance Monoid (AutomatonFromChoice ac a) 
  => Monoid (AutomatonStepHistoryQuantum ac a)
deriving newtype instance MonoidStar (AutomatonFromChoice ac a) 
  => MonoidStar (AutomatonStepHistoryQuantum ac a)
deriving newtype instance ChoiceSemigroup (AutomatonFromChoice ac a) 
  => ChoiceSemigroup (AutomatonStepHistoryQuantum ac a)

instance (Ord t, CreatesBellPairs (sq t) t, Pointed (AutomatonFromChoice ac))
        => CreatesBellPairs (AutomatonStepHistoryQuantum ac (sq t)) t where
    tryCreateBellPairFrom = point . tryCreateBellPairFrom

instance (Ord t, ChoiceSemigroup (sq t), Quantum (sq t) t)
        => Quantum (AutomatonStepHistoryQuantum 'ACEmbedded (sq t)) t where

instance (Ord t, Ord (sq t), Quantum (sq t) t)
        => Quantum (AutomatonStepHistoryQuantum 'ACNormal (sq t)) t where

instance (ChoiceSemigroup (sq t), Quantum (sq t) t) 
        => OrderedQuantum (AutomatonStepHistoryQuantum 'ACEmbedded (sq t)) t where
    newtype Layer (AutomatonStepHistoryQuantum 'ACEmbedded (sq t)) = OneStepEmbedded (sq t)
    orderedTryCreateBellPairFrom = OneStepEmbedded . tryCreateBellPairFrom
    liftLayer (OneStepEmbedded s) = point s

instance (Ord (sq t), Quantum (sq t) t) 
        => OrderedQuantum (AutomatonStepHistoryQuantum 'ACNormal (sq t)) t where
    newtype Layer (AutomatonStepHistoryQuantum 'ACNormal (sq t)) = OneStepNormal (sq t)
    orderedTryCreateBellPairFrom = OneStepNormal . tryCreateBellPairFrom
    liftLayer (OneStepNormal s) = point s

instance (Semigroup (sq t)) => OrderedSemigroup (Layer (AutomatonStepHistoryQuantum 'ACEmbedded (sq t))) where
   (OneStepEmbedded s) <.> (OneStepEmbedded s') = OneStepEmbedded (s <> s')

instance (Semigroup (sq t)) => OrderedSemigroup (Layer (AutomatonStepHistoryQuantum 'ACNormal (sq t))) where
   (OneStepNormal s) <.> (OneStepNormal s') = OneStepNormal (s <> s')

instance (Ord tag, ChoiceSemigroup (sq tag), TestsQuantum (sq tag) test tag)
        => TestsOrderedQuantum (AutomatonStepHistoryQuantum 'ACEmbedded (sq tag)) test tag where
    orderedTest = OneStepEmbedded . test

executeE :: (Ord b, Show b)
    => (a -> b -> Set b)
    -> AutomatonStepHistoryQuantum 'ACEmbedded a
    -> b -> Set b
executeE executeStep ahq = fromJust . executeWithE (AE.EP Nothing) executeStep ahq

executeWithE :: (Ord b, Show b)
    => ExecutionParams
    -> (a -> b -> Set b)
    -> AutomatonStepHistoryQuantum 'ACEmbedded a
    -> b -> Maybe (Set b)
executeWithE params executeStep (AutomatonStepHistoryQuantum nfa) = AE.execute params executeStep nfa
