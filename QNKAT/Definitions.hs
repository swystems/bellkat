{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module QNKAT.Definitions where 

import Data.String (IsString)
import Data.Tree (Forest, Tree (Node), rootLabel)
import Data.Bifunctor (first)
import Data.List (partition)
import Control.Monad ((>=>))
import Data.Maybe (fromMaybe)

-- * Type definitions

newtype Location = Location { name :: String } deriving newtype (Eq, Show, IsString)

-- | `:~:` is our symbol for entangled pair
data BellPair = Location :~: Location

-- | how BPs are displayed
instance Show BellPair where
    show (l1 :~: l2) = name l1 <> "," <> name l2
instance Eq BellPair where
    l1 :~: l2 == l1' :~: l2' = (l1 , l2) == (l1', l2') || (l2, l1) == (l1', l2') 

-- parallel composition is left-associative
infixl <||>

-- | Define alg structure `ParallelSemigroup` with `<>` inherited from 
-- `Semigroup` and new `<||>` for parallel composition
class Semigroup a => ParallelSemigroup a where
    (<||>) :: a -> a -> a

-- | `Quantum` is a `ParallelSemigroup` with `BellPair` creation
class (ParallelSemigroup a) => Quantum a where
    -- | is function from `BellPair`, `[BellPair]` to `Maybe Double` Quantum;
    -- | will be used in `meaning` of `Distill`
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
    deriving newtype (Show, Semigroup, Monoid) 

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
findTreeRoots (p : ps) h = findTreeRoot p h >>= \(t, h) ->
    findTreeRoots ps h >>= \(ts, h) -> return (t : ts, h)
    
findSubHistory :: [BellPair] -> History -> Maybe (History, History)
findSubHistory ps h = first History <$> findTreeRoots ps h

findSubHistory' :: [BellPair] -> History -> (Maybe History, History)
findSubHistory' ps h = maybe (Nothing, h) (first return) $ findSubHistory ps h

dup :: History -> History
dup = History . map (\t -> Node (rootLabel t) [t]) . getForest

-- ** Quantum operations represented as functions over histories

data HistoryQuantum = HistoryQuantum 
    { requiredRoots :: [BellPair]
    , execute :: History -> [History] 
    }
        
-- | executes on compatible roots and leaves the rest intact
executePartial 
    :: HistoryQuantum -> History -> ([History], History)
executePartial hq h = 
    case findSubHistory (requiredRoots hq) h of
        Nothing -> ([History[]], h)
        Just (hInput, hRest) -> (execute hq hInput, hRest)

instance Semigroup HistoryQuantum where
    -- | Definition of `<>` as sequential composition of `execute`
    hq <> hq' = HistoryQuantum 
        { requiredRoots = requiredRoots hq
        , execute = execute hq >=> execute hq'
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
        { requiredRoots = bp
        , execute = \h -> fromMaybe [dup h] $ findTreeRoots bp h >>= \(ts, h) -> return $
            case prob of 
                Nothing -> [dup h <> History [Node p ts]]
                Just _ -> [dup h <> History [Node p ts], dup h]
        }

applyPolicy :: Policy -> History -> [History]
applyPolicy = execute . meaning