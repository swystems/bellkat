{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE TupleSections      #-}

module QNKAT.ChoiceUtilities where

import           Data.Foldable              (toList)
import           Data.Functor.Contravariant (Predicate (..))
import           Data.List                  (elemIndex, partition, permutations)
import           Data.Maybe                 (fromJust)
import qualified Data.Multiset              as Mset
import           Data.Set                   (Set)
import qualified Data.Set                   as Set

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
choose _ [] = []
choose n (x:xs) = [chooseAll [x] <> p | p <- choose (n - 1) xs] ++ [chooseNoneOf [x] <> p | p <- choose n xs]

findTreeRootsP :: (Ord a) => Predicate a -> UForest a -> Partial (UForest a)
findTreeRootsP p ts = 
     Partial 
         { chosen = Mset.filter (getPredicate p . rootLabel) ts
         , rest = Mset.filter (not. getPredicate p . rootLabel) ts
         }

-- | choose subforest with the given roots based on keys
findTreeRootsNDP :: (Ord a, Eq k) => (a -> k) -> [k] -> Predicate a -> UForest a -> [Partial (UForest a)]
findTreeRootsNDP key ks p ts = 
    let filtered = findTreeRootsP p ts
    in [ ts' { rest = rest ts' <> rest filtered } 
       | ts' <- findTreeRootsNDWithKey key ks (chosen filtered) ]

findTreeRootsNDWithKey :: (Ord a, Eq k) => (a -> k) -> [k] -> UForest a -> [Partial (UForest a)]
findTreeRootsNDWithKey _ [] ts = [chooseNoneOf ts]
findTreeRootsNDWithKey key ks@(k:_) ts =
    let (curKeys, restKeys) = partition (== k) ks
        curTrees = Mset.filter ((== k) . key . rootLabel) ts
        restTrees = Mset.filter ((/= k) . key . rootLabel) ts
     in [fmap Mset.fromList ts' <> ts''
            | ts' <- choose (length curKeys) (toList curTrees)
            , ts'' <- findTreeRootsNDWithKey key restKeys restTrees]

findTreeRootsND :: (Ord a) => [a] -> UForest a -> [Partial (UForest a)]
findTreeRootsND [] ts = [chooseNoneOf ts]
findTreeRootsND bps@(bp:_) ts =
    let (curBps, restBps) = partition (== bp) bps
        curTrees = Mset.filter (hasRoot bp) ts
        restTrees = Mset.filter (not . hasRoot bp) ts
     in [fmap Mset.fromList ts' <> ts''
            | ts' <- choose (length curBps) (toList curTrees)
            , ts'' <- findTreeRootsND restBps restTrees]

-- | choose a separate subforest for each set of roots
findTreeRootsAnyND :: (Ord a) => [[a]] -> UForest a -> [Partial (UForest a)]
findTreeRootsAnyND [] h = [chooseNoneOf h]
findTreeRootsAnyND (ps : pss) h =
    [partialH' { chosen = chosen partialH <> chosen partialH' }
      | partialH <- chooseNoneOfIfEmpty h $ findTreeRootsND ps h
      , partialH' <- findTreeRootsAnyND pss (rest partialH)]


-- | _non-deterministically_ _try_ to choose a separate subforest for each set of roots _one by one_
chooseTreesSequentialND :: (Ord a) => [[a]] -> UForest a -> Set [Maybe (UForest a)]
chooseTreesSequentialND pss = chooseTreesSequentialNDP id (withTruePredicate pss)
--
-- | _non-deterministically_ _try_ to choose a separate subforest for each set of roots _one by one_ with a predicate
chooseTreesSequentialNDP
    :: (Ord a, Eq k) 
    => (a -> k) -> [([k], Predicate a)] -> UForest a -> Set [Maybe (UForest a)]
chooseTreesSequentialNDP _key [] _ = [[]]
chooseTreesSequentialNDP key ((ps, p):pss) ts =
    case findTreeRootsNDP key ps p ts of
      [] -> Set.map (Nothing:) (chooseTreesSequentialNDP key pss ts)
      ts' -> mconcat
        [ Set.map (Just here:) $ chooseTreesSequentialNDP key pss there
          | Partial { chosen = here, rest = there } <- ts'
        ]

-- | _non-deterministically_ _try_ to choose a separate subforest for each set of roots with a predicate
chooseTreesNDP
    :: (Ord a, Eq k)
    => (a -> k) -> [([k], Predicate a)]
    -> UForest a -> Set [Maybe (UForest a)]
chooseTreesNDP key pss ts =
    mconcat
    [ Set.map (applyPermutation $ inversePermutation ixs)
            $ chooseTreesSequentialNDP key (applyPermutation ixs pss) ts
    | ixs <- permutations [0..length pss - 1]]

-- | _non-deterministically_ _try_ to choose a separate subforest for each set of roots
chooseTreesND
    :: (Ord a) => [[a]] -> UForest a -> Set [Maybe (UForest a)]
chooseTreesND pss = chooseTreesNDP id (withTruePredicate pss)
  --
-- | choose two subforests _non_deterministically_ with a predicate
chooseTwoSubforestsP
    :: (Ord a, Eq k)
    => (a -> k)
    -> [([k], Predicate a)] -> [([k], Predicate a)]
    -> UForest a -> Set (UForest a, UForest a, UForest a)
chooseTwoSubforestsP key reqRoots1 reqRoots2 ts =
    let n1 = length reqRoots1
        n2 = length reqRoots2
        combine mbhs = mconcat $ concatMap toList mbhs
        split mbhs =
            let ts1 = combine (take n1 mbhs)
                ts2 = combine (take n2 . drop n1 $ mbhs)
             in (ts1, ts2, ts `Mset.difference` ts1 `Mset.difference` ts2)
     in Set.fromList
        [ split p
        | p <- Set.toList $ chooseTreesNDP key (reqRoots1 <> reqRoots2) ts
        ]

-- | choose two subforests _non_deterministically_
chooseTwoSubforests
    :: (Ord a)
    => [[a]] -> [[a]] -> UForest a -> Set (UForest a, UForest a, UForest a)
chooseTwoSubforests reqRoots1 reqRoots2 =
     chooseTwoSubforestsP id (withTruePredicate reqRoots1) (withTruePredicate reqRoots2)

withTruePredicate :: [[a]] -> [([a], Predicate a)]
withTruePredicate = map (, mempty)

-- | Apply a permutation to a list
applyPermutation :: [Int] -> [a] -> [a]
applyPermutation ixs xs = map (xs !!) ixs

-- | Computes an inverse permutation
inversePermutation :: [Int] -> [Int]
inversePermutation ixs = [ fromJust $ elemIndex i ixs  | i <- [0..length ixs - 1]]
