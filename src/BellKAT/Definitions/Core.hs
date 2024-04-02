{-# LANGUAGE OverloadedLists #-}
module BellKAT.Definitions.Core (
    Location,
    TaggedRequiredRoots,
    CreateBellPairArgs(..),
    BellPair(..),
    hasLocation,
    History(..),
    DupKind(..),
    Test(..),
    BellPairsPredicate(..),
    RestrictedTest,  
    createRestrictedTest,
    (.+.),
    (.&&.),
    Partial(..),
    TaggedBellPair(..),
    TaggedBellPairs,
    FreeTest(..),
    dupHistory,
    dupForest,
    dupHistoryN,
    processDup,
    chooseKHistories,
    ) where

import           Data.Bifunctor             (bimap)
import           Data.Foldable              (toList)
import           Data.Functor.Contravariant (Predicate (..), (>$<))
import           Data.Functor.Classes
import           Data.List                  (sort)
import           Data.Monoid                (Endo (..))
import qualified Data.Multiset              as Mset
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.String                (IsString)
import qualified GHC.Exts                   (IsList, Item, fromList, toList)

import           Data.Vector.Fixed          (Arity, VecList)
import qualified Data.Vector.Fixed          as FV
import           Test.QuickCheck            hiding (choose, (.&&.))
import           Data.Multiset              (Multiset)

import           BellKAT.Utils.Choice
import           BellKAT.Utils.UnorderedTree
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

hasLocation :: Location -> BellPair -> Bool
hasLocation l (l1 :~: l2) = l == l1 || l == l2

data DupKind = DupKind { dupBefore :: Bool, dupAfter :: Bool }

instance Semigroup DupKind where
    (DupKind x y) <> (DupKind x' y') = DupKind (x || x') (y || y')

instance Monoid DupKind where
    mempty = DupKind False False

data CreateBellPairArgs tag = CreateBellPairArgs
    { cbpPredicate   :: Predicate tag
    , cbpOutputBP    :: BellPair -- ^ a produced (output) `BellPair`
    , cbpInputBPs    :: [BellPair] -- ^ a multiset of required (input) `BellPair`s
    , cbpProbability :: Maybe Double -- ^ probability of failure for operations that may fail
    , cbtNewTag      :: tag
    , cbtDup         :: DupKind
    }

instance Show1 CreateBellPairArgs where
  liftShowsPrec _ _ _ _ = shows "cbp"

data FreeTest t
    = FTSubset (TaggedBellPairs t)
    | FTNot (FreeTest t)

instance Show1 FreeTest where
    liftShowsPrec s sl i (FTNot t) = showString "~" . liftShowsPrec s sl i t
    liftShowsPrec _ _ _ (FTSubset bps) = shows (map bellPair $ toList bps)

instance Show t => Show (FreeTest t) where
    showsPrec _ (FTSubset x) = shows x
    showsPrec d (FTNot x) = showParen (app_prec < d) $ showString "not " . shows x
      where app_prec = 10

newtype RestrictedTest tag = RestrictedTest [TaggedBellPairs tag]
    deriving newtype (Eq, Ord)

createRestrictedTest :: Ord tag => [TaggedBellPairs tag] -> RestrictedTest tag
createRestrictedTest = RestrictedTest . sort . restrictedTestNormalize

restrictedTestNormalize :: Ord tag => [TaggedBellPairs tag] -> [TaggedBellPairs tag]
restrictedTestNormalize [] = []
restrictedTestNormalize (x:xs) = 
    if any (`Mset.isSubsetOf` x) xs 
       then restrictedTestNormalize xs
       else x:restrictedTestNormalize xs

instance (Show tag) => Show (RestrictedTest tag) where
    showsPrec _ (RestrictedTest []) = showString "TRUE"
    showsPrec _ (RestrictedTest (x:xs)) = shows (toList x) . showsRest xs
      where 
        showsRest [] = id
        showsRest (y : ys) = showString " /\\ " . shows (toList y) . showsRest ys

(.+.) :: (Ord tag) => RestrictedTest tag -> TaggedBellPairs tag -> RestrictedTest tag
(RestrictedTest bpss) .+. bps = createRestrictedTest (map (<> bps) bpss)

(.&&.) :: (Ord tag) => RestrictedTest tag -> RestrictedTest tag -> RestrictedTest tag
(RestrictedTest bpss) .&&. (RestrictedTest bpss') = 
    createRestrictedTest (bpss <> bpss')

instance Test RestrictedTest where
    toBPsPredicate (RestrictedTest s) = BPsPredicate $ \bps -> not (any (`Mset.isSubsetOf` bps) s)

newtype BellPairsPredicate t = BPsPredicate { getBPsPredicate :: TaggedBellPairs t -> Bool }

class Test test where
    toBPsPredicate :: Ord tag => test tag -> BellPairsPredicate tag

instance Test FreeTest where
    toBPsPredicate (FTNot t) = BPsPredicate $ not . getBPsPredicate (toBPsPredicate t)
    toBPsPredicate (FTSubset bps) = BPsPredicate (bps `Mset.isSubsetOf`)

instance Test BellPairsPredicate where
    toBPsPredicate = id

instance Show (BellPairsPredicate t) where
  showsPrec _ _ = shows "test"

-- * History of BellPairs

type RequiredRoots = [BellPair]

data TaggedBellPair t = TaggedBellPair
    { bellPair    :: BellPair
    , bellPairTag :: t
    } deriving stock (Eq, Ord)

instance Show t => Show (TaggedBellPair t) where
    showsPrec _ (TaggedBellPair bp t) = shows bp . showString "/" . shows t


type TaggedRequiredRoots t = (RequiredRoots, Predicate t)

-- | `TaggedBellPairs` is a multiset of Bell pairs
type TaggedBellPairs tag = Multiset (TaggedBellPair tag)

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
requiredRootsToPredicate (x, y) = (x, bellPairTag >$< y)

-- | choose k subhistories _non_deterministically_
chooseKHistories
    :: (Ord t, Arity n)
    => VecList n [TaggedRequiredRoots t]
    -> History t -> Set (VecList n (History t), History t)
chooseKHistories reqRoots (History ts) =
      Set.map (bimap (FV.map History) History)
  $ chooseKSubforestsP bellPair (FV.map (map requiredRootsToPredicate) reqRoots) ts

-- ** History duplication utilities

-- | Duplicating history
dupHistory :: Ord t => History t -> History t
dupHistory = History . dupForest . getForest

dupForest :: Ord t => UForest t -> UForest t
dupForest = Mset.map (\t -> Node (rootLabel t) [t])

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

instance (Arbitrary t) => Arbitrary (TaggedBellPair t) where
    arbitrary = TaggedBellPair <$> arbitrary <*> arbitrary
