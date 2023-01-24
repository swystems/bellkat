{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ParallelListComp           #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FunctionalDependencies     #-}

module QNKAT.Definitions where

import           Data.Foldable              (toList)
import           Data.Functor.Contravariant (Predicate (..), (>$<))
import           Data.List                  (foldl', sort)
import           Data.Monoid
import qualified Data.Multiset              as Mset
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.String                (IsString)
import qualified GHC.Exts                   (IsList, Item, fromList, toList)
import           Test.QuickCheck            hiding (choose)

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

newtype DupKind = DupKind { shouldDup :: Bool }
    deriving newtype Arbitrary

-- | `Quantum` is a `ParallelSemigroup` with `BellPair` creation
class (ParallelSemigroup a) => Quantum a t | a -> t where
    -- | is function from `BellPair`, `[BellPair]` to `Maybe Double` Quantum;
    -- will be used in `meaning` of `Distill`
    tryCreateBellPairFrom 
        :: Predicate t -- | tag predicate
        -> BellPair 
        -> [BellPair] 
        -> Maybe Double 
        -> t -- | new tag
        -> DupKind
        -> a

-- | Notation for predicate
subjectTo :: Quantum a t => Predicate t -> (Predicate t -> a) -> a
subjectTo pt f = f pt

-- | Notation for deterministic `tryCreateBellPairFrom`
(<~) :: (Quantum a t) => BellPair -> [BellPair] -> t -> DupKind -> Predicate t -> a
bp <~ bps = \t dk pt -> tryCreateBellPairFrom pt bp bps Nothing t dk

-- | Notation for probabilistic `tryCreateBellPairFrom` (part I)
(<~%) :: BellPair -> Double -> (BellPair, Double)
bp <~% prob = (bp, prob)

-- | Notation for probabilistic `tryCreateBellPairFrom` (part II)
(%~) :: Quantum a t => (BellPair, Double) -> [BellPair] -> t -> DupKind -> Predicate t -> a
(bp, prob) %~ bps = \t dk pt -> tryCreateBellPairFrom pt bp bps (Just prob) t dk

-- * Main policy definitions

-- | Define primitive actions
data Action 
    = Swap Location (Location, Location)
    | Transmit Location (Location, Location)
    | Distill (Location, Location)
    | Create Location
    deriving stock (Show)

data TaggedAction t = TaggedAction
    { taTagPredicate :: Predicate t
    , taAction :: Action
    , taTag :: t
    , taDup :: DupKind
    }

instance Show t => Show (TaggedAction t) where
    show ta = "_:" <> show (taAction ta) <> ":" <> show (taTag ta)

-- | Define policy
data Policy t
    = Atomic (TaggedAction t)
    | Sequence (Policy t) (Policy t)
    | Parallel (Policy t) (Policy t)
    deriving stock (Show)

instance Semigroup (Policy t) where
    (<>) = Sequence

instance ParallelSemigroup (Policy t) where
    (<||>) = Parallel

-- | methods
meaning :: Quantum a t => Policy t -> a
meaning (Atomic ta) = case taAction ta of
    (Swap l (l1, l2))     -> subjectTo (taTagPredicate ta) 
        $ ((l1 :~: l2) <~ [l :~: l1, l :~: l2]) (taTag ta) (taDup ta)
    (Transmit l (l1, l2)) -> subjectTo (taTagPredicate ta) 
        $ ((l1 :~: l2) <~ [l :~: l]) (taTag ta) (taDup ta)
    (Create l)            -> subjectTo (taTagPredicate ta) 
        $ ((l :~: l) <~ []) (taTag ta) (taDup ta)
    (Distill (l1, l2))    -> subjectTo (taTagPredicate ta) 
        $ ((l1 :~: l2) <~% 0.5 %~ [l1 :~: l2, l1 :~: l2]) (taTag ta) (taDup ta)
meaning (Sequence p q)        = meaning p <> meaning q
meaning (Parallel p q)        = meaning p <||> meaning q

-- * History of BellPairs

type RequiredRoots = [BellPair]
type TaggedBellPair t = (BellPair, t)
type TaggedRequiredRoots t = (RequiredRoots, Predicate t)

-- | `History` is a forest of `BellPair`s
newtype History t = History { getForest :: UForest (TaggedBellPair t) }
    deriving newtype (Semigroup, Monoid, Eq, Ord, Arbitrary)

instance (Ord t) => GHC.Exts.IsList (History t) where
    type Item (History t) = UTree (TaggedBellPair t)
    toList = toList . getForest
    fromList = History . GHC.Exts.fromList

instance (Ord t, Show t) => Show (History t) where
    show = show . GHC.Exts.toList

-- | Duplicating history
dupHistory :: Ord t => History t -> History t
dupHistory = History . Mset.map (\t -> Node (rootLabel t) [t]) . getForest

-- | Duplicating history
dupHistoryN :: Ord t => Int -> History t -> History t
dupHistoryN n = appEndo . mconcat . replicate n $ Endo dupHistory

processDup :: Ord a => DupKind -> UForest a -> UForest a
processDup dk ts
  | shouldDup dk = ts
  | otherwise = foldMap subForest ts

mapPredicate :: [TaggedRequiredRoots t] 
             -> [(RequiredRoots, Predicate (TaggedBellPair t))]
mapPredicate = map (\(x, y) -> (x, snd >$< y))

-- | choose two subhistory _non_deterministically_
chooseTwoHistories 
    :: Ord t
    => [TaggedRequiredRoots t] 
    -> [TaggedRequiredRoots t] 
    -> History t -> Set (History t, History t, History t)
chooseTwoHistories reqRoots1 reqRoots2 (History ts) = 
      Set.map (\(a, b, c) -> (History a, History b, History c))
  $ chooseTwoSubforestsP fst (mapPredicate reqRoots1) (mapPredicate reqRoots2) ts
  

-- ** Quantum operations represented as functions over histories

data HistoryQuantum t = HistoryQuantum
    { requiredRoots :: [TaggedRequiredRoots t]
    , execute       :: History t -> Set (History t)
    }

instance Ord t => Semigroup (HistoryQuantum t) where
    -- | Definition of `<>` as sequential composition of `execute`
    hq <> hq' = HistoryQuantum
        { requiredRoots = requiredRoots hq
        , execute = \h -> Set.fromList [h'' | h' <- Set.elems $ execute hq h,  h'' <- Set.elems $ execute hq' h']
        }

instance Ord t => ParallelSemigroup (HistoryQuantum t) where
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

instance Ord t => Quantum (HistoryQuantum t) t where
    tryCreateBellPairFrom pt p bp prob t dk = HistoryQuantum
        { requiredRoots = [(bp, pt)]
        , execute = \h@(History ts) ->
            case findTreeRootsNDP fst bp (snd >$< pt) ts of
                [] -> [h]
                partialTsNews ->
                    mconcat
                    [ case prob of
                        Nothing -> [History tsRest <> [Node (p, t) . processDup dk $ tsNew]]
                        Just _ -> [History tsRest <> [Node (p, t) . processDup dk $ tsNew], History tsRest]
                    | Partial { chosen = tsNew, rest = tsRest } <- partialTsNews
                    ]
        }

applyPolicy :: Ord t => Policy t -> History t -> Set (History t)
applyPolicy = execute . meaning

-- ** Quantum operations represented as functions over histories recorded in a
-- _timely_ manner

type Time = Int

data TimelyHistoryQuantum t = TimelyHistoryQuantum
    { requiredRootsTimely :: [TaggedRequiredRoots t]
    , executeTimely       :: History t -> Set (History t, Time)
    }

instance Ord t => Semigroup (TimelyHistoryQuantum t) where
    -- | Definition of `<>` as sequential composition of `execute`
    hq <> hq' = TimelyHistoryQuantum
        { requiredRootsTimely = requiredRootsTimely hq
        , executeTimely = \h -> Set.fromList
            [(h'', t' + t'')
            | (h', t') <- Set.elems $ executeTimely hq h
            ,  (h'', t'') <- Set.elems $ executeTimely hq' h']
        }

instance Ord t => ParallelSemigroup (TimelyHistoryQuantum t) where
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

instance Ord t => Quantum (TimelyHistoryQuantum t) t where
    tryCreateBellPairFrom pt bp bps prob t _dk = TimelyHistoryQuantum
        { requiredRootsTimely = [(bps, pt)]
        , executeTimely = \h@(History ts) ->
            case findTreeRootsNDP fst bps (snd >$< pt) ts of
                [] -> [(dupHistory h, 1)]
                partialTsNews -> mconcat
                    [ case prob of
                        Nothing -> [(dupHistory (History tsRest) <> [Node (bp, t) tsNew], 1)]
                        Just _ ->  [(dupHistory (History tsRest) <> [Node (bp, t) tsNew], 1)
                                   ,(dupHistory (History tsRest), 1)]
                    | Partial { chosen = tsNew, rest = tsRest } <- partialTsNews
                    ]
        }

applyPolicyTimely :: Ord t => Policy t -> History t -> Set (History t)
applyPolicyTimely p = Set.map fst . executeTimely (meaning p)

-- ** Quantum operations represented as a sequence of primitive actions

newtype StepHistoryQuantum t = StepHistoryQuantum
    { getSteps :: [TimelyHistoryQuantum t]
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

applyPolicySteps :: (Ord t) => Policy t -> History t -> Set (History t)
applyPolicySteps p h = foldl'
    (\hs hq -> Set.unions (Set.map (Set.map fst . executeTimely hq) hs))
    (Set.singleton h) (getSteps $ meaning p)

-- * Testing definitions

instance Arbitrary Location where
    arbitrary = Location <$> growingElements [[c] | c <- ['A'..'Z']]

instance Arbitrary Action where
    arbitrary = oneof 
        [ Swap <$> arbitrary <*> arbitrary
        , Create <$> arbitrary
        , Transmit <$> arbitrary <*> arbitrary
        , Distill <$> arbitrary
        ]


instance (Arbitrary t, Eq t) => Arbitrary (Policy t) where
    arbitrary = do
        n <- getSize
        if n == 0 then
            resize 1 $ do
                predicate :: [t] <- arbitrary
                Atomic <$> (TaggedAction (Predicate (`elem` predicate)) <$> arbitrary <*> arbitrary <*> arbitrary)
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
