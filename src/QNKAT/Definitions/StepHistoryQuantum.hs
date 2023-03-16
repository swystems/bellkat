{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE StrictData       #-}
{-# LANGUAGE TypeApplications #-}
module QNKAT.Definitions.StepHistoryQuantum (execute) where

import           Data.List              (foldl')
import           Data.Set               (Set)
import qualified Data.Set               as Set

import           QNKAT.Definitions.Core

-- ** Quantum operations represented as a sequence of primitive actions
newtype StepHistoryQuantum sq t = StepHistoryQuantum
    { getSteps :: [[sq t]]
    }

instance ParallelSemigroup (sq t) =>  ParallelSemigroup (StepHistoryQuantum sq t) where
    shq <||> shq' = StepHistoryQuantum $ [ steps <|||> steps' | steps <- getSteps shq, steps' <- getSteps shq']

instance Semigroup (StepHistoryQuantum sq t) where
    shq <> shq' = StepHistoryQuantum $ (<>) <$> getSteps shq <*> getSteps shq'

instance Monoid (StepHistoryQuantum sq t) where
    mempty = StepHistoryQuantum [[]]

(<|||>) :: ParallelSemigroup (sq t) => [sq t] -> [sq t] -> [sq t]
steps <|||> steps' =
    let shortestLength = minimum @[] $ length <$> [steps, steps']
        commonSteps = take shortestLength
        restSteps = drop shortestLength
    in [hq <||> hq' | hq <- commonSteps steps | hq' <- commonSteps steps']
            ++ restSteps steps ++ restSteps steps'

instance (Ord t, Quantum (sq t) t) => Quantum (StepHistoryQuantum sq t) t where
    tryCreateBellPairFrom args = StepHistoryQuantum [[tryCreateBellPairFrom args]]

instance (Ord t, Quantum (sq t) t) => OrderedQuantum (StepHistoryQuantum sq t) t where
    newtype Layer (StepHistoryQuantum sq t) = OneStep (sq t)

    orderedTryCreateBellPairFrom = OneStep . tryCreateBellPairFrom

    fromLayer (OneStep s) = StepHistoryQuantum [[s]]

instance (Semigroup (sq t)) => OrderedSemigroup (Layer (StepHistoryQuantum sq t)) where
   (OneStep s) <.> (OneStep s') = OneStep (s <> s')

instance ChoiceSemigroup (StepHistoryQuantum sq t) where
    hq <+> hq' = StepHistoryQuantum $ getSteps hq <> getSteps hq'

instance (Ord t, TestsQuantum (sq t) t) => TestsOrderedQuantum (StepHistoryQuantum sq t) t where
    orderedTest = OneStep . test

execute :: Ord t
    => (sq t -> History t -> Set (History t))
    -> StepHistoryQuantum sq t
    -> History t -> Set (History t)
execute executeStep shq h = mconcat
    [ foldl' (\hs hq -> Set.unions (Set.map (executeStep hq) hs)) (Set.singleton h) steps
    | steps <- getSteps shq
    ]
