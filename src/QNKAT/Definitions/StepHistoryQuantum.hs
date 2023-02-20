{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE StrictData       #-}
{-# LANGUAGE TypeApplications #-}
module QNKAT.Definitions.StepHistoryQuantum (execute) where

import           Data.List                        (foldl')
import           Data.Set                         (Set)
import qualified Data.Set                         as Set

import           QNKAT.Definitions.Core

-- ** Quantum operations represented as a sequence of primitive actions
newtype StepHistoryQuantum sq t = StepHistoryQuantum
    { getSteps :: [sq t]
    } deriving newtype (Semigroup)

instance (Ord t, ParallelSemigroup (sq t)) =>  ParallelSemigroup (StepHistoryQuantum sq t) where
    shq <||> shq' = StepHistoryQuantum $
        let shortestLength = minimum @[] $ length . getSteps <$> [shq, shq']
            commonSteps = take shortestLength . getSteps
            restSteps = drop shortestLength . getSteps
         in [hq <||> hq' | hq <- commonSteps shq | hq' <- commonSteps shq']
                ++ restSteps shq ++ restSteps shq'

instance (Ord t, Quantum (sq t) t) => Quantum (StepHistoryQuantum sq t) t where
    tryCreateBellPairFrom pt p bp prob t dk = StepHistoryQuantum
        [tryCreateBellPairFrom pt p bp prob t dk]

execute :: Ord t => (sq t -> History t -> Set (History t)) -> StepHistoryQuantum sq t -> History t -> Set (History t)
execute executeStep shq h = foldl' (\hs hq -> Set.unions (Set.map (executeStep hq) hs)) (Set.singleton h) $ getSteps shq
