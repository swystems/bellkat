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
import qualified Data.Multiset as Mset
import Data.Multiset (Multiset)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified GHC.Exts (IsList, Item, toList, fromList)
import Data.Foldable (toList)
import Data.Set (Set)

import QNKAT.UnorderedTree
import QNKAT.ChoiceUtilities

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

-- ** Duplicating history

dup :: History -> History
dup = History . Mset.map (\t -> Node (rootLabel t) [t]) . getForest

-- ** Quantum operations represented as functions over histories

data HistoryQuantum = HistoryQuantum 
    { requiredRoots :: [[BellPair]]
    , execute :: History -> Set History
    }

-- | choose two subhistory _non_deterministically_
chooseTwoHistories :: [[BellPair]] -> [[BellPair]] -> History -> Set (History, History, History)
chooseTwoHistories reqRoots1 reqRoots2 (History ts) 
  = Set.map (\(a, b, c) -> (History a, History b, History c))
  $ chooseTwoSubforests reqRoots1 reqRoots2 ts

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
