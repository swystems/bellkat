{-# LANGUAGE StrictData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module BellKAT.Implementations.AutomataStepHistoryQuantum 
    ( AutomatonStepHistoryQuantum (getNFA)
    , executeE
    , executeWithE
    , executeWith
    , execute
    , ExecutionParams(..)
    ) where

import           Data.Pointed
import           Data.List.NonEmpty             (NonEmpty)
import           Data.Set                       (Set)
import           Data.Maybe                     (fromJust)
 

import           BellKAT.Definitions.Structures
import           BellKAT.Implementations.Automata
import qualified BellKAT.Implementations.AutomataExecution as AE
import           BellKAT.Implementations.AutomataExecution (ExecutionParams)
import           BellKAT.Utils.NonEmpty

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

    
instance (Ord t, CreatesBellPairs (sq t) t, ChoiceSemigroup (sq t), Pointed (AutomatonFromChoice 'ACEmbedded))
        => CreatesBellPairs (AutomatonStepHistoryQuantum 'ACEmbedded (sq t)) t where
    tryCreateBellPairFrom = point . tryCreateBellPairFrom

instance (Ord t, Ord (sq t), CreatesBellPairs (NonEmpty (sq t)) t, Pointed (AutomatonFromChoice 'ACNormal))
        => CreatesBellPairs (AutomatonStepHistoryQuantum 'ACNormal (sq t)) t where
    tryCreateBellPairFrom = foldNonEmpty (<+>) . fmap point . tryCreateBellPairFrom

instance (Ord t, ChoiceSemigroup (sq t), Quantum (sq t) t, CreatesBellPairs (sq t) t)
        => Quantum (AutomatonStepHistoryQuantum 'ACEmbedded (sq t)) t where

instance (Ord t, Ord (sq t), ParallelSemigroup (sq t), CreatesBellPairs (NonEmpty (sq t)) t)
        => Quantum (AutomatonStepHistoryQuantum 'ACNormal (sq t)) t where

instance (ChoiceSemigroup (sq t), Quantum (sq t) t) 
        => OrderedLayeredQuantum (AutomatonStepHistoryQuantum 'ACEmbedded (sq t)) t where
    newtype Layer (AutomatonStepHistoryQuantum 'ACEmbedded (sq t)) = OneStep (sq t)
    orderedTryCreateBellPairFrom = OneStep . tryCreateBellPairFrom
    liftLayer (OneStep s) = point s

instance (Ord t, Ord (sq t), ParallelSemigroup (sq t), OrderedSemigroup (sq t), CreatesBellPairs (NonEmpty (sq t)) t) 
        => OrderedQuantum (AutomatonStepHistoryQuantum 'ACNormal (sq t)) t where

instance (Semigroup (sq t)) => OrderedSemigroup (Layer (AutomatonStepHistoryQuantum 'ACEmbedded (sq t))) where
   (OneStep s) <.> (OneStep s') = OneStep (s <> s')

instance (Ord tag, Tests (sq tag) test tag)
        => Tests (AutomatonStepHistoryQuantum 'ACNormal (sq tag)) test tag where
    test = point . test

instance (Ord t, Ord (sq t), Tests (sq t) test t, ParallelSemigroup (sq t), OrderedSemigroup (sq t), CreatesBellPairs (NonEmpty (sq t)) t) 
        => TestsOrderedQuantum (AutomatonStepHistoryQuantum 'ACNormal (sq t)) test t where

instance (Ord tag, ChoiceSemigroup (sq tag), TestsQuantum (sq tag) test tag)
        => TestsOrderedLayeredQuantum (AutomatonStepHistoryQuantum 'ACEmbedded (sq tag)) test tag where
    orderedTest = OneStep . test

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

executeWith :: (Ord b, Show b)
    => ExecutionParams
    -> (a -> b -> Set b)
    -> AutomatonStepHistoryQuantum 'ACNormal a
    -> b -> Maybe (Set b)
executeWith params executeStep (AutomatonStepHistoryQuantum nfa) = AE.executeHyper params executeStep nfa

execute :: (Ord b, Show b)
    => (a -> b -> Set b)
    -> AutomatonStepHistoryQuantum 'ACNormal a
    -> b -> Set b
execute executeStep ahq = fromJust . executeWith (AE.EP Nothing) executeStep ahq
