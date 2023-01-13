{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ParallelListComp           #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}

module QNKAT.Definitions where

import           Data.Foldable         (toList)
import           Data.List             (sort, foldl')
import           Data.Monoid
import qualified Data.Multiset         as Mset
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.String           (IsString)
import qualified GHC.Exts              (IsList, Item, fromList, toList)
import           Test.QuickCheck       hiding (choose)

import           QNKAT.ChoiceUtilities
import           QNKAT.UnorderedTree

-- * Type definitions

newtype Location = Location { name :: String } deriving newtype (Eq, Show, Ord, IsString)

-- | `:~:` is our symbol for entangled pair
data BellPair = Location :~: Location

infix 9 :~:

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
    dup :: a

-- | Notation for deterministic `tryCreateBellPairFrom`
(<~) :: (Quantum a) => BellPair -> [BellPair] -> a
bp <~ bps = tryCreateBellPairFrom bp bps Nothing

-- | Notation for probabilistic `tryCreateBellPairFrom` (part I)
(<~%) :: BellPair -> Double -> (BellPair, Double)
bp <~% prob = (bp, prob)

-- | Notation for probabilistic `tryCreateBellPairFrom` (part II)
(%~) :: (Quantum a) => (BellPair, Double) -> [BellPair] -> a
(bp, prob) %~ bps = tryCreateBellPairFrom bp bps (Just prob)

-- * Main policy definitions

-- | Define policy
data Policy
    = Swap Location (Location, Location)
    | Transmit Location (Location, Location)
    | Distill (Location, Location)
    | Sequence Policy Policy
    | Parallel Policy Policy
    | Create Location
    | Dup
    deriving stock (Show)

instance Semigroup Policy where
    (<>) = Sequence

instance ParallelSemigroup Policy where
    (<||>) = Parallel

-- | methods
meaning :: (Quantum a) => Policy -> a
meaning (Swap l (l1, l2))     = (l1 :~: l2) <~ [l :~: l1, l :~: l2]
meaning (Transmit l (l1, l2)) = (l1 :~: l2) <~ [l :~: l]
meaning (Create l)            = (l :~: l) <~ []
meaning (Distill (l1, l2))    = (l1 :~: l2) <~% 0.5 %~ [l1 :~: l2, l1 :~: l2]
meaning (Sequence p q)        = meaning p <> meaning q
meaning (Parallel p q)        = meaning p <||> meaning q
meaning Dup                   = dup

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

-- | Duplicating history
dupHistory :: History -> History
dupHistory = History . Mset.map (\t -> Node (rootLabel t) [t]) . getForest

-- | Duplicating history
dupHistoryN :: Int -> History -> History
dupHistoryN n = appEndo . mconcat . replicate n $ Endo dupHistory

-- | choose two subhistory _non_deterministically_
chooseTwoHistories :: [[BellPair]] -> [[BellPair]] -> History -> Set (History, History, History)
chooseTwoHistories reqRoots1 reqRoots2 (History ts)
  = Set.map (\(a, b, c) -> (History a, History b, History c))
  $ chooseTwoSubforests reqRoots1 reqRoots2 ts

-- ** Quantum operations represented as functions over histories

data HistoryQuantum = HistoryQuantum
    { requiredRoots :: [[BellPair]]
    , execute       :: History -> Set History
    }

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
                [ hRest <> hNew <> hNew'
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
                [] -> [h]
                partialTsNews ->
                    mconcat
                    [ case prob of
                        Nothing -> [History tsRest <> [Node p . foldMap subForest . toList $ tsNew]]
                        Just _ -> [History tsRest <> [Node p . foldMap subForest . toList $ tsNew], History tsRest]
                    | Partial { chosen = tsNew, rest = tsRest } <- partialTsNews
                    ]
        }

    dup = HistoryQuantum { requiredRoots = [], execute = \h -> [dupHistory h] }

applyPolicy :: Policy -> History -> Set History
applyPolicy = execute . meaning

-- ** Quantum operations represented as functions over histories recorded in a
-- _timely_ manner

type Time = Int

data TimelyHistoryQuantum = TimelyHistoryQuantum
    { requiredRootsTimely :: [[BellPair]]
    , executeTimely       :: History -> Set (History, Time)
    }

instance Semigroup TimelyHistoryQuantum where
    -- | Definition of `<>` as sequential composition of `execute`
    hq <> hq' = TimelyHistoryQuantum
        { requiredRootsTimely = requiredRootsTimely hq
        , executeTimely = \h -> Set.fromList
            [(h'', t' + t'')
            | (h', t') <- Set.elems $ executeTimely hq h
            ,  (h'', t'') <- Set.elems $ executeTimely hq' h']
        }

instance ParallelSemigroup TimelyHistoryQuantum where
    --- | Definition of `<||>` as parallel composition
    hq <||> hq' = TimelyHistoryQuantum
        { requiredRootsTimely = requiredRootsTimely hq <> requiredRootsTimely hq'
        , executeTimely = \h ->
            Set.fromList
                [ (dupHistoryN (max t t') hRest
                    <> dupHistoryN (max t t' - t) hNew <> dupHistoryN (max t t' - t') hNew'
                  , max t t')
                | (hs, hs', hRest) <- toList $
                    chooseTwoHistories (requiredRootsTimely hq) (requiredRootsTimely hq') h
                , (hNew, t) <- Set.elems $ executeTimely hq hs
                , (hNew', t') <- Set.elems $ executeTimely hq' hs'
                ]
        }

instance Quantum TimelyHistoryQuantum where
    tryCreateBellPairFrom p bp prob = TimelyHistoryQuantum
        { requiredRootsTimely = [bp]
        , executeTimely = \h@(History ts) ->
            case findTreeRootsND bp ts of
                [] -> [(dupHistory h, 1)]
                partialTsNews -> mconcat
                    [ case prob of
                        Nothing -> [(dupHistory (History tsRest) <> [Node p tsNew], 1)]
                        Just _ ->  [(dupHistory (History tsRest) <> [Node p tsNew], 1)
                                   ,(dupHistory (History tsRest), 1)]
                    | Partial { chosen = tsNew, rest = tsRest } <- partialTsNews
                    ]
        }

    dup = TimelyHistoryQuantum { requiredRootsTimely = [], executeTimely = \h -> [(h, 0)] }

applyPolicyTimely :: Policy -> History -> Set History
applyPolicyTimely p = Set.map fst . executeTimely (meaning p)

-- ** Quantum operations represented as a sequence of primitive actions

newtype StepHistoryQuantum = StepHistoryQuantum
    { getSteps :: [TimelyHistoryQuantum]
    } deriving newtype (Semigroup)

instance ParallelSemigroup StepHistoryQuantum where
    shq <||> shq' = StepHistoryQuantum $
        let shortestLength = minimum @[] $ length . getSteps <$> [shq, shq']
            commonSteps = take shortestLength . getSteps
            restSteps = drop shortestLength . getSteps
         in [hq <||> hq' | hq <- commonSteps shq | hq' <- commonSteps shq'] 
                ++ restSteps shq ++ restSteps shq'

instance Quantum StepHistoryQuantum where
    tryCreateBellPairFrom p bp prob = StepHistoryQuantum
        [tryCreateBellPairFrom p bp prob]

    dup = StepHistoryQuantum []

applyPolicySteps :: Policy -> History -> Set History
applyPolicySteps p h = foldl' 
    (\hs hq -> Set.unions (Set.map (Set.map fst . executeTimely hq) hs)) 
    (Set.singleton h) (getSteps $ meaning p)

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
    shrink _              = []

instance Arbitrary BellPair where
    arbitrary = (:~:) <$> arbitrary <*> arbitrary
