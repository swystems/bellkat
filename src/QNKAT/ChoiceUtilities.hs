{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists    #-}

module QNKAT.ChoiceUtilities where

import           Data.Foldable       (toList)
import           Data.List           (elemIndex, partition, permutations)
import           Data.Maybe          (fromJust)
import qualified Data.Multiset       as Mset
import           Data.Set            (Set)
import qualified Data.Set            as Set

import           QNKAT.UnorderedTree

-- | Part that we have chosen and part that we have left out
data Partial a = Partial { chosen :: a, rest :: a }
    deriving stock (Show, Eq)

chooseAll :: Monoid a => a -> Partial a
chooseAll x = Partial { chosen = x, rest = mempty }

chooseNoneOf :: Monoid a => a -> Partial a
chooseNoneOf x = Partial { chosen = mempty, rest = x }

chooseNoneOfIfEmpty :: (Monoid a) => a -> [Partial a] -> [Partial a]
chooseNoneOfIfEmpty x [] = [chooseNoneOf x]
chooseNoneOfIfEmpty _ xs = xs

instance Semigroup a => Semigroup (Partial a) where
    p <> p' = Partial { chosen = chosen p <> chosen p', rest = rest p <> rest p' }

instance Functor Partial where
    fmap f p = Partial { chosen = f (chosen p), rest = f (rest p) }


-- | choose k items non-deterministically
choose :: (Ord a) => Int -> [a] -> [Partial [a]]
choose 0 xs = [chooseNoneOf xs]
choose n [] = []
choose n (x:xs) = [chooseAll [x] <> p | p <- choose (n - 1) xs] ++ [chooseNoneOf [x] <> p | p <- choose n xs]

-- | choose subforest with the given roots
findTreeRootsND :: (Ord a) => [a] -> UForest a -> [Partial (UForest a)]
findTreeRootsND [] ts = [chooseNoneOf ts]
findTreeRootsND bps@(bp:_) ts =
    let (curBps, restBps) = partition (== bp) bps
        curTrees = Mset.filter (hasRoot bp) ts
        restTrees = Mset.filter (not . hasRoot bp) ts
     in [fmap Mset.fromList ts <> ts'
            | ts <- choose (length curBps) (toList curTrees)
            , ts' <- findTreeRootsND restBps restTrees]

-- | choose a separate subforest for each set of roots
findTreeRootsAnyND :: (Ord a) => [[a]] -> UForest a -> [Partial (UForest a)]
findTreeRootsAnyND [] h = [chooseNoneOf h]
findTreeRootsAnyND (ps : pss) h =
    [partialH' { chosen = chosen partialH <> chosen partialH' }
      | partialH <- chooseNoneOfIfEmpty h $ findTreeRootsND ps h
      , partialH' <- findTreeRootsAnyND pss (rest partialH)]


-- | _non-deterministically_ _try_ to choose a separate subforest for each set of roots _one by one_
chooseTreesSequentialND :: (Ord a) => [[a]] -> UForest a -> Set [Maybe (UForest a)]
chooseTreesSequentialND [] _ = [[]]
chooseTreesSequentialND (ps:pss) ts =
    case findTreeRootsND ps ts  of
      [] -> Set.map (Nothing:) (chooseTreesSequentialND pss ts)
      ts -> mconcat
        [ Set.map (Just here:) $ chooseTreesSequentialND pss there
          | Partial { chosen = here, rest = there } <- ts
        ]

-- | _non-deterministically_ _try_ to choose a separate subforest for each set of roots
chooseTreesND
    :: (Ord a) => [[a]] -> UForest a -> Set [Maybe (UForest a)]
chooseTreesND pss ts =
    mconcat
    [ Set.map (applyPermutation $ inversePermutation ixs)
            $ chooseTreesSequentialND (applyPermutation ixs pss) ts
    | ixs <- permutations [0..length pss - 1]]
      --
-- | choose two subforests _non_deterministically_
chooseTwoSubforests
    :: (Ord a) => [[a]] -> [[a]] -> UForest a -> Set (UForest a, UForest a, UForest a)
chooseTwoSubforests reqRoots1 reqRoots2 ts =
    let n1 = length reqRoots1
        n2 = length reqRoots2
        combine mbhs = mconcat $ concatMap toList mbhs
        split mbhs =
            let ts1 = combine (take n1 mbhs)
                ts2 = combine (take n2 . drop n1 $ mbhs)
             in (ts1, ts2, ts `Mset.difference` ts1 `Mset.difference` ts2)
     in Set.fromList
        [ split partition
        | partition <- Set.toList $ chooseTreesND (reqRoots1 <> reqRoots2) ts
        ]

-- | Apply a permutation to a list
applyPermutation :: [Int] -> [a] -> [a]
applyPermutation ixs xs = map (xs !!) ixs

-- | Computes an inverse permutation
inversePermutation :: [Int] -> [Int]
inversePermutation ixs = [ fromJust $ elemIndex i ixs  | i <- [0..length ixs - 1]]
