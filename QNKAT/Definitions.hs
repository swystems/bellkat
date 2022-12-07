{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module QNKAT.Definitions where 

import Data.String (IsString)
import Data.Tree (Forest, Tree (Node), rootLabel)
import Data.List (partition)
import Test.QuickCheck

-- * Type definitions

newtype Location = Location { name :: String } deriving newtype (Eq, Show, IsString)

-- | `:~:` is our symbol for entangled pair
data BellPair = Location :~: Location

-- | how BPs are displayed
instance Show BellPair where
    show (l1 :~: l2) = name l1 <> "~" <> name l2
instance Eq BellPair where
    l1 :~: l2 == l1' :~: l2' = (l1 , l2) == (l1', l2') || (l2, l1) == (l1', l2') 

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
newtype History = History { getForest :: Forest BellPair } 
    deriving newtype (Show, Semigroup, Monoid, Eq, Arbitrary) 

-- ** A few helper functions to operate on histories

-- | Partitions the history into (first) tree whose root matches `p` and other
-- trees
findTreeRoot :: BellPair -> History -> Maybe (Tree BellPair, History)
findTreeRoot p (History ts) =
    case partition ((== p) . rootLabel) ts of
        (t:ts, ts') -> Just (t, History (ts <> ts'))
        _ -> Nothing

findTreeRoots :: [BellPair] -> History -> Maybe (Forest BellPair, History)
findTreeRoots [] h = Just ([], h)
findTreeRoots (p : ps) h = 
    case findTreeRoot p h of
        Nothing -> Nothing                         
        Just (t, h) -> case findTreeRoots ps h of 
                         Nothing -> Nothing
                         Just (ts, h) -> return (t : ts, h)

data PartialHistory = PartialHistory { chosen :: History, rest :: History }

findSubHistory :: [BellPair] -> History -> Maybe PartialHistory
findSubHistory ps h = 
    case findTreeRoots ps h of
      Nothing -> Nothing
      Just (ts, h) -> Just PartialHistory { chosen = History ts, rest = h }

findSubHistoryAny :: [[BellPair]] -> History -> Maybe PartialHistory
findSubHistoryAny [] h = Nothing
findSubHistoryAny (ps : pss) h = 
    case findSubHistory ps h of
      Nothing -> findSubHistoryAny pss h
      Just PartialHistory { chosen = h, rest = hRest } -> 
          case findSubHistoryAny pss hRest of
            Nothing -> Just PartialHistory { chosen = h, rest = hRest }
            Just PartialHistory { chosen = h', rest = hRest' } ->
                Just PartialHistory { chosen = h <> h', rest = hRest' }

dup :: History -> History
dup = History . map (\t -> Node (rootLabel t) [t]) . getForest

-- ** Quantum operations represented as functions over histories

data HistoryQuantum = HistoryQuantum 
    { requiredRoots :: [[BellPair]]
    , execute :: History -> [History] 
    }
        
-- | executes on compatible roots and leaves the rest intact
executePartial 
    :: HistoryQuantum -> History -> ([History], History)
executePartial hq h = 
    case findSubHistoryAny (requiredRoots hq) h of
        Nothing -> ([History[]], h)
        Just PartialHistory { chosen = hInput, rest = hRest } -> 
            (execute hq hInput, hRest)

instance Semigroup HistoryQuantum where
    -- | Definition of `<>` as sequential composition of `execute`
    hq <> hq' = HistoryQuantum 
        { requiredRoots = requiredRoots hq
        , execute = \h -> [h'' | h' <- execute hq h, h'' <- execute hq' h']
        }
    
instance ParallelSemigroup HistoryQuantum where
    --- | Definition of `<||>` as parallel composition
    hq <||> hq' = HistoryQuantum
        { requiredRoots = requiredRoots hq <> requiredRoots hq'
        , execute = \h -> 
            let (hs, hRest) = executePartial hq h
                (hs', hRest') = executePartial hq' hRest
            in [dup hRest' <> hNew <> hNew' | hNew <- hs , hNew' <- hs']
        }
    
instance Quantum HistoryQuantum where
    tryCreateBellPairFrom p bp prob = HistoryQuantum 
        { requiredRoots = [bp]
        , execute = \h -> 
            case findTreeRoots bp h of
                Nothing -> [dup h]
                Just (ts, h) -> 
                    case prob of 
                        Nothing -> [dup h <> History [Node p ts]]
                        Just _ -> [dup h <> History [Node p ts], dup h]
        }

applyPolicy :: Policy -> History -> [History]
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
