{-# LANGUAGE OverloadedLists #-}
module QNKAT.Definitions.Core where

import           Data.Bifunctor             (bimap)
import           Data.Foldable              (toList)
import           Data.Functor.Contravariant (Predicate (..), (>$<))
import           Data.Monoid                (Endo(..))
import           Data.List                  (sort)
import qualified Data.Multiset              as Mset
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.String                (IsString)
import qualified GHC.Exts                   (IsList, Item, fromList, toList)

import           Data.Vector.Fixed          (Arity, VecList)
import qualified Data.Vector.Fixed          as FV
import           Test.QuickCheck            hiding (choose)

import           QNKAT.ChoiceUtilities
import           QNKAT.UnorderedTree
--
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

data DupKind = DupKind { dupBefore :: Bool, dupAfter :: Bool }

instance Semigroup DupKind where
    (DupKind x y) <> (DupKind x' y') = DupKind (x || x') (y || y')

instance Monoid DupKind where
    mempty = DupKind False False

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

-- ** History choice utilities

requiredRootsToPredicate :: TaggedRequiredRoots t -> (RequiredRoots, Predicate (TaggedBellPair t))
requiredRootsToPredicate (x, y) = (x, snd >$< y)

-- | choose k subhistories _non_deterministically_
chooseKHistories
    :: (Ord t, Arity n)
    => VecList n [TaggedRequiredRoots t]
    -> History t -> Set (VecList n (History t), History t)
chooseKHistories reqRoots (History ts) =
      Set.map (bimap (FV.map History) History)
  $ chooseKSubforestsP fst (FV.map (map requiredRootsToPredicate) reqRoots) ts

-- ** History duplication utilities

-- | Duplicating history
dupHistory :: Ord t => History t -> History t
dupHistory = History . Mset.map (\t -> Node (rootLabel t) [t]) . getForest

-- | Duplicating history
dupHistoryN :: Ord t => Int -> History t -> History t
dupHistoryN n = appEndo . mconcat . replicate n $ Endo dupHistory

processDupAfter :: Ord a => Bool -> a -> UForest a -> UTree a
processDupAfter True x ts  = Node x [Node x ts]
processDupAfter False x ts = Node x ts

processDupBefore :: Ord a => Bool -> UForest a -> UForest a
processDupBefore True ts  = ts
processDupBefore False ts = foldMap subForest ts

processDup :: Ord a => DupKind -> a -> UForest a -> UTree a
processDup dk x = processDupAfter (dupAfter dk) x . processDupBefore (dupBefore dk)


-- * Testing definitions

instance Arbitrary Location where
    arbitrary = Location <$> growingElements [[c] | c <- ['A'..'Z']]

instance Arbitrary BellPair where
    arbitrary = (:~:) <$> arbitrary <*> arbitrary

instance Arbitrary DupKind where
    arbitrary = DupKind <$> arbitrary <*> arbitrary
