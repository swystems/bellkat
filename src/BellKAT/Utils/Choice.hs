{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies      #-}

module BellKAT.Utils.Choice where

import           Data.Foldable              (toList)
import           Data.Functor.Contravariant (Predicate (..))
import           Data.List                  (elemIndex, partition, permutations)
import           Data.Maybe                 (fromJust)
import qualified Data.Multiset              as Mset
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Vector.Fixed          (VecList, Arity)
import qualified Data.Vector.Fixed          as FV

import           BellKAT.Utils.UnorderedTree

-- | Part that we have chosen and part that we have left out
data Partial a = Partial { chosen :: a, rest :: a }
    deriving stock (Show, Eq, Ord)

chooseAll :: Monoid a => a -> Partial a
chooseAll x = Partial { chosen = x, rest = mempty }

chooseNoneOf :: Monoid a => a -> Partial a
chooseNoneOf x = Partial { chosen = mempty, rest = x }

chooseNoneOfIfEmpty :: (Monoid a) => a -> [Partial a] -> [Partial a]
chooseNoneOfIfEmpty x [] = [chooseNoneOf x]
chooseNoneOfIfEmpty _ xs = xs

mapChosen :: (a -> a) -> Partial a -> Partial a
mapChosen f p = p { chosen = f (chosen p) }

instance Semigroup a => Semigroup (Partial a) where
    p <> p' = Partial { chosen = chosen p <> chosen p', rest = rest p <> rest p' }

instance Functor Partial where
    fmap f p = Partial { chosen = f (chosen p), rest = f (rest p) }

-- | merge back chosen and rest
unchoose :: (Semigroup a) => Partial a -> a
unchoose pa = chosen pa <> rest pa

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

-- | choose k subforests _non_deterministically_ with a predicate
chooseKSubforestsP
    :: (Ord a, Eq k, Arity n)
    => (a -> k)
    -> VecList n [([k], Predicate a)]
    -> UForest a -> Set (VecList n (UForest a), UForest a)
chooseKSubforestsP key reqRoots ts =
    let ns = FV.map length reqRoots
        nsAcc = FV.scanl1 (+) ns
        combine mbhs = mconcat $ concatMap toList mbhs
        split mbhs =
            let tss = FV.zipWith (\n k -> combine (take k . drop (n - k) $ mbhs)) nsAcc ns 
                tssRest = FV.foldl Mset.difference ts tss
             in (tss, tssRest)
     in Set.fromList
        [ split p
        | p <- Set.toList $ chooseTreesNDP key (FV.fold reqRoots) ts
        ]

-- | choose two subforests _non_deterministically_
chooseKSubforests
    :: (Ord a, Arity n)
    => VecList n [[a]] -> UForest a -> Set (VecList n (UForest a), UForest a)
chooseKSubforests reqRoots =
     chooseKSubforestsP id (FV.map withTruePredicate reqRoots)

withTruePredicate :: [[a]] -> [([a], Predicate a)]
withTruePredicate = map (, mempty)

-- | Apply a permutation to a list
applyPermutation :: [Int] -> [a] -> [a]
applyPermutation ixs xs = map (xs !!) ixs

-- | Computes an inverse permutation
inversePermutation :: [Int] -> [Int]
inversePermutation ixs = [ fromJust $ elemIndex i ixs  | i <- [0..length ixs - 1]]
