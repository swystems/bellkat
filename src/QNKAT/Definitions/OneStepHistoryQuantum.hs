{-# LANGUAGE OverloadedLists #-}
module QNKAT.Definitions.OneStepHistoryQuantum (execute) where

import           Data.Foldable          (foldl', toList)
import           Data.List              (permutations)
import           Data.Set               (Set)
import qualified Data.Set               as Set

import           QNKAT.ChoiceUtilities
import           QNKAT.Definitions.Core

newtype OneStepHistoryQuantum t = OneStepHistoryQuantum
    { threads :: [History t -> Set (Partial (History t))]
    }

bindRest :: (Ord a, Monoid a) => Set (Partial a) -> (a -> Set (Partial a)) -> Set (Partial a)
bindRest spa ma = Set.fromList [ pb <> chooseAll (chosen pa) | pa <- toList spa, pb <- toList $ ma (rest pa)  ]

executeOneStep :: Ord t => OneStepHistoryQuantum t -> History t -> Set (Partial (History t))
executeOneStep hq h = mconcat [ foldl' bindRest [chooseNoneOf h] ts 
                              | ts <- permutations (threads hq) ]

instance (Ord t) => Semigroup (OneStepHistoryQuantum t) where
    hq <> hq' = OneStepHistoryQuantum 
        [\h -> [chooseNoneOf h] `bindRest` executeOneStep hq `bindRest` executeOneStep hq']

instance (Ord t) => ParallelSemigroup (OneStepHistoryQuantum t) where
    hq <||> hq' = OneStepHistoryQuantum (threads hq <> threads hq')

execute :: Ord t => OneStepHistoryQuantum t -> History t -> Set (History t)
execute hq = Set.map unchoose . executeOneStep hq
