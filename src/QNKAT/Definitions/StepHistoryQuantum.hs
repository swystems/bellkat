{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE StrictData       #-}
{-# LANGUAGE TypeApplications #-}
module QNKAT.Definitions.StepHistoryQuantum (execute) where

import           Data.List                        (foldl')
import           Data.Set                         (Set)
import qualified Data.Set                         as Set

import           QNKAT.Definitions.Core
import           QNKAT.Definitions.HistoryQuantum (HistoryQuantum)
import qualified QNKAT.Definitions.HistoryQuantum as HQ

-- ** Quantum operations represented as a sequence of primitive actions
newtype StepHistoryQuantum t = StepHistoryQuantum
    { getSteps :: [HistoryQuantum t]
    } deriving newtype (Semigroup)

instance Ord t => ParallelSemigroup (StepHistoryQuantum t) where
    shq <||> shq' = StepHistoryQuantum $
        let shortestLength = minimum @[] $ length . getSteps <$> [shq, shq']
            commonSteps = take shortestLength . getSteps
            restSteps = drop shortestLength . getSteps
         in [hq <||> hq' | hq <- commonSteps shq | hq' <- commonSteps shq']
                ++ restSteps shq ++ restSteps shq'

instance Ord t => Quantum (StepHistoryQuantum t) t where
    tryCreateBellPairFrom pt p bp prob t dk = StepHistoryQuantum
        [tryCreateBellPairFrom pt p bp prob t dk]

execute :: Ord t => History t -> StepHistoryQuantum t -> Set (History t)
execute h = foldl' (\hs hq -> Set.unions (Set.map (HQ.execute hq) hs)) (Set.singleton h) . getSteps
