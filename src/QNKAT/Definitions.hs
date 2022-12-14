{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module QNKAT.Definitions where 

import Data.String (IsString)
import Data.List (partition, sort, permutations, elemIndex)
import Test.QuickCheck hiding (choose)
import QNKAT.UnorderedTree
import qualified Data.Multiset as Mset
import Data.Multiset (Multiset)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified GHC.Exts (IsList, Item, toList, fromList)
import Data.Maybe (fromJust)
import Data.Foldable (toList)
import Data.Set (Set)

-- * Type definitions

newtype Location = Location { name :: String } deriving newtype (Eq, Show, Ord, IsString)

-- | `:~:` is our symbol for entangled pair
data BellPair = Location :~: Location

-- | how BPs are displayed
instance Show BellPair where
    show (l1 :~: l2) = name l1 <> "~" <> name l2
instance Eq BellPair where
    l1 :~: l2 == l1' :~: l2' = sort [l1, l2] == sort [l2', l1']

instance Ord BellPair where
    compare (l1 :~: l2) (l1' :~: l2') = compare (sort [l1, l2]) (sort [l1', l2'])

-- parallel composition is left-associative
infixl 5 <||>

-- | Define alg structure `ParallelSemigroup` with `<>` inherited from 
-- `Semigroup` and new `<||>` for parallel composition
class Semigroup a => ParallelSemigroup a where
    (<||>) :: a -> a -> a

-- | `Quantum` is a `ParallelSemigroup` with `BellPair` creation
class (ParallelSemigroup a) => Quantum a where
    -- | is function from `BellPair`, `[BellPair]` to `Maybe Double` Quantum;
    -- will be used in `meaning` of `Distill`
    tryCreateBellPairFrom :: BellPair -> [BellPair] -> Maybe Double -> a 

-- | A deterministic version of `tryCreateBellPairFrom`
createBellPairFrom :: (Quantum a) => BellPair -> [BellPair] -> a
createBellPairFrom bp bps = tryCreateBellPairFrom bp bps Nothing

-- * Main policy definitions

-- | Define policy
data Policy
    = Swap Location (Location, Location)
    | Transmit Location (Location, Location)
    | Distill (Location, Location)
    | Sequence Policy Policy
    | Parallel Policy Policy
    | Create Location 
    deriving stock (Show)

instance Semigroup Policy where
    (<>) = Sequence

instance ParallelSemigroup Policy where
    (<||>) = Parallel

-- | methods
meaning :: (Quantum a) => Policy -> a
meaning (Swap l (l1, l2)) = createBellPairFrom (l1 :~: l2) [l :~: l1, l :~: l2]
meaning (Transmit l (l1, l2)) = createBellPairFrom (l1 :~: l2) [l :~: l]
meaning (Create l) = createBellPairFrom (l :~: l) []
meaning (Distill (l1, l2)) = tryCreateBellPairFrom (l1 :~: l2) [l1 :~: l2, l1 :~: l2] (Just 0.5)
meaning (Sequence p q) = meaning p <> meaning q
meaning (Parallel p q) = meaning p <||> meaning q

-- * History of BellPairs

-- | `History` is a forest of `BellPair`s
newtype History = History { getForest :: UForest BellPair } 
    deriving newtype (Semigroup, Monoid, Eq, Ord, Arbitrary) 

instance GHC.Exts.IsList History where
    type Item History = UTree BellPair
    toList = toList . getForest
    fromList = History . GHC.Exts.fromList


instance Show History where
    show = show . GHC.Exts.toList


-- ** A few helper functions to operate on histories

-- | Part that we have chosen and part that we have left out
data Partial a = Partial { chosen :: a, rest :: a }
    deriving stock (Show, Eq)

chooseAll :: Monoid a => a -> Partial a
chooseAll x = Partial { chosen = x, rest = mempty }

chooseNoneOf :: Monoid a => a -> Partial a
chooseNoneOf x = Partial { chosen = mempty, rest = x }

instance Semigroup a => Semigroup (Partial a) where
    p <> p' = Partial { chosen = chosen p <> chosen p', rest = rest p <> rest p' }

instance Functor Partial where
    fmap f p = Partial { chosen = f (chosen p), rest = f (rest p) }

hasRoot :: BellPair -> UTree BellPair -> Bool
hasRoot p = (== p) . rootLabel

-- | Partitions the history into *first* tree whose root matches `p` and other
-- trees
findTreeRoot :: BellPair -> UForest BellPair -> Maybe (UTree BellPair, UForest BellPair)
findTreeRoot p ts =
    case Mset.elems . Mset.filter (hasRoot p) $ ts of
      (t:_) -> Just (t, Mset.remove t ts)
      [] -> Nothing

findTreeRoots :: [BellPair] -> UForest BellPair -> Maybe (Partial (UForest BellPair))
findTreeRoots [] ts = Just $ chooseNoneOf ts
findTreeRoots (p : ps) ts = 
    case findTreeRoot p ts of
        Nothing -> Nothing                         
        Just (t, ts) -> case findTreeRoots ps ts of 
                         Nothing -> Nothing
                         Just p -> Just $ chooseAll [t] <> p

findSubHistory :: [BellPair] -> History -> Maybe (Partial History)
findSubHistory ps (History ts) = 
    case findTreeRoots ps ts of
      Nothing -> Nothing
      Just p -> Just $ fmap History p

findSubHistoryAny :: [[BellPair]] -> History -> Maybe (Partial History)
findSubHistoryAny [] h = Nothing
findSubHistoryAny (ps : pss) h = 
    case findSubHistory ps h of
      Nothing -> findSubHistoryAny pss h
      Just partialH -> 
          case findSubHistoryAny pss (rest partialH) of
            Nothing -> Just partialH
            Just partialH' ->
                Just partialH' { chosen = chosen partialH <> chosen partialH' }

-- *** Non-deterministic versions

choose :: (Ord a) => Int -> [a] -> [Partial [a]]
choose 0 xs = [chooseNoneOf xs]
choose n [] = []
choose n (x:xs) = [chooseAll [x] <> p | p <- choose (n - 1) xs] ++ [chooseNoneOf [x] <> p | p <- choose n xs]

findTreeRootsND :: [BellPair] -> UForest BellPair -> [Partial (UForest BellPair)]
findTreeRootsND [] ts = [chooseNoneOf ts]
findTreeRootsND bps@(bp:_) ts = 
    let (curBps, restBps) = partition (== bp) bps
        curTrees = Mset.filter (hasRoot bp) ts
        restTrees = Mset.filter (not . hasRoot bp) ts
     in [fmap Mset.fromList ts <> ts' 
            | ts <- choose (length curBps) (toList curTrees)
            , ts' <- findTreeRootsND restBps restTrees]

findSubHistoryND :: [BellPair] -> History -> [Partial History]
findSubHistoryND ps (History ts) = [fmap History pts | pts <- findTreeRootsND ps ts]

chooseNoneOfIfEmpty :: (Monoid a) => a -> [Partial a] -> [Partial a]
chooseNoneOfIfEmpty x [] = [chooseNoneOf x]
chooseNoneOfIfEmpty _ xs = xs

findSubHistoryAnyND :: [[BellPair]] -> History -> [Partial History]
findSubHistoryAnyND [] h = [chooseNoneOf h]
findSubHistoryAnyND (ps : pss) h = 
    [partialH' { chosen = chosen partialH <> chosen partialH' } 
      | partialH <- chooseNoneOfIfEmpty h $ findSubHistoryND ps h 
      , partialH' <- findSubHistoryAnyND pss (rest partialH)]

-- *** Duplicating history

dup :: History -> History
dup = History . Mset.map (\t -> Node (rootLabel t) [t]) . getForest

-- ** Quantum operations represented as functions over histories

data HistoryQuantum = HistoryQuantum 
    { requiredRoots :: [[BellPair]]
    , execute :: History -> Set History
    }

chooseHistoriesSequential :: [[BellPair]] -> History -> Set [Maybe History]
chooseHistoriesSequential [] _ = [[]]
chooseHistoriesSequential (ps:pss) h = 
    case findSubHistoryND ps h  of 
      [] -> Set.map (Nothing:) (chooseHistoriesSequential pss h)
      hs -> mconcat
        [ Set.map (Just here:) $ chooseHistoriesSequential pss there 
          | Partial { chosen = here, rest = there } <- hs 
        ]

inversePermutation :: [Int] -> [Int]
inversePermutation ixs = [ fromJust $ elemIndex i ixs  | i <- [0..length ixs - 1]]

-- | choose subhistories _non_deterministically_
chooseHistories :: [[BellPair]] -> History -> Set [Maybe History]
chooseHistories pss h = mconcat 
    [ Set.map (\mhs -> map (mhs !!) $ inversePermutation ixs) $ chooseHistoriesSequential (map (pss !!) ixs) h 
        | ixs <- permutations [0..length pss - 1]]

        
-- | choose two subhistory _non_deterministically_
chooseTwoHistories :: [[BellPair]] -> [[BellPair]] -> History -> Set (History, History, History)
chooseTwoHistories reqRoots1 reqRoots2 h@(History ts) = 
    let n1 = length reqRoots1
        n2 = length reqRoots2
        combine mbhs = mconcat $ concatMap toList mbhs
        split mbhs = 
            let h1@(History ts1) = combine (take n1 mbhs)
                h2@(History ts2) = combine (take n2 . drop n1 $ mbhs)
             in (h1, h2, History $ ts `Mset.difference` ts1 `Mset.difference` ts2)
     in Set.fromList [ split partition 
        | partition <- Set.toList $ chooseHistories (reqRoots1 <> reqRoots2) h 
        ]

instance Semigroup HistoryQuantum where
    -- | Definition of `<>` as sequential composition of `execute`
    hq <> hq' = HistoryQuantum 
        { requiredRoots = requiredRoots hq
        , execute = \h -> Set.fromList [h'' | h' <- Set.elems $ execute hq h,  h'' <- Set.elems $ execute hq' h']
        }
    
instance ParallelSemigroup HistoryQuantum where
    --- | Definition of `<||>` as parallel composition
    hq <||> hq' = HistoryQuantum
        { requiredRoots = requiredRoots hq <> requiredRoots hq'
        , execute = \h -> 
            Set.fromList 
                [ dup hRest <> hNew <> hNew'
                    | (hs, hs', hRest) <- toList $ 
                        chooseTwoHistories (requiredRoots hq) (requiredRoots hq') h
                    , hNew <- Set.elems $ execute hq hs
                    , hNew' <- Set.elems $ execute hq' hs'
                ]
        }
    
instance Quantum HistoryQuantum where
    tryCreateBellPairFrom p bp prob = HistoryQuantum 
        { requiredRoots = [bp]
        , execute = \h@(History ts) ->
            case findTreeRootsND bp ts of
                [] -> [dup h]
                partialTsNews -> mconcat
                    [ case prob of 
                        Nothing -> [dup (History tsRest) <> [Node p tsNew]]
                        Just _ -> [dup (History tsRest) <> [Node p tsNew], dup (History tsRest)]
                    | Partial { chosen = tsNew, rest = tsRest } <- partialTsNews
                    ]
        }

applyPolicy :: Policy -> History -> Set History
applyPolicy = execute . meaning

-- * Testing definitions

instance Arbitrary Location where
    arbitrary = Location <$> growingElements [[c] | c <- ['A'..'Z']]

instance Arbitrary Policy where
    arbitrary = do
        n <- getSize
        if n == 0 then
            resize 1 $ oneof [
                Swap <$> arbitrary <*> arbitrary,
                Create <$> arbitrary,
                Transmit <$> arbitrary <*> arbitrary,
                Distill <$> arbitrary
            ]
        else
            resize (n - 1) $ oneof [
                Sequence <$> arbitrary <*> arbitrary,
                Sequence <$> arbitrary <*> arbitrary
            ]
    shrink (Sequence p q) = [p, q]
    shrink (Parallel p q) = [p, q]
    shrink _ = []

instance Arbitrary BellPair where
    arbitrary = (:~:) <$> arbitrary <*> arbitrary
