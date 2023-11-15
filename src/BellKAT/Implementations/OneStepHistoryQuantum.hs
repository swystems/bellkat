{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE UndecidableInstances #-}
module BellKAT.Implementations.OneStepHistoryQuantum 
    ( OneStepPolicy(..)
    , OneStep
    , OneStepFree
    , execute
    , executePartial
    ) where

import           Data.Foldable                (toList)
import           Data.Functor.Compose         (Compose (..))
import           Data.Functor.Contravariant   ((>$<))
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.Multiset                as Mset
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Functor.Classes

import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Structures
import           BellKAT.Utils.Choice
import           BellKAT.Utils.UnorderedTree    (UTree (..))

data OneStepPolicy a
    = Atomic a
    | Sequence (OneStepPolicy a) (OneStepPolicy a)
    | Choice (OneStepPolicy a) (OneStepPolicy a)
    deriving stock (Show)

instance Show1 OneStepPolicy where
  liftShowsPrec sp _ d (Atomic x) = sp d x
  liftShowsPrec sp sl d (Sequence x y) = 
      showsBinaryWith (liftShowsPrec sp sl) (liftShowsPrec sp sl) "Sequence" d x y
  liftShowsPrec sp sl d (Choice x y) = 
      showsBinaryWith (liftShowsPrec sp sl) (liftShowsPrec sp sl) "Choice" d x y

instance Semigroup (OneStepPolicy a) where
    (<>) = Sequence

instance (Monoid a) => Monoid (OneStepPolicy a) where
    mempty = Atomic mempty

chooseConcat :: NonEmpty (OneStepPolicy a) -> OneStepPolicy a
chooseConcat (x :| [])       = x
chooseConcat (x :| x' : xs') = Choice x (chooseConcat (x' :| xs'))

instance ParallelSemigroup (OneStepPolicy a) where
    p <||> q = chooseConcat $
        fmap (intermixAfterDecompsition q) (decompose p)
        <> fmap (intermixAfterDecompsition p) (decompose q)

instance OrderedSemigroup (OneStepPolicy a) where
    p <.> q = chooseConcat $
        fmap (intermixAfterDecompsition q) (decompose p)
        <> fmap (intermixAfterDecompsition p) (decompose q)

type Decomposition a = Either a (a, OneStepPolicy a)

decompose :: OneStepPolicy a -> NonEmpty (Either a (a, OneStepPolicy a))
decompose (Atomic x)     = [Left x]
decompose (Choice x y)   = decompose x <> decompose y
decompose (Sequence p q) = fmap (sequenceAfterDecomposition q) (decompose p)

intermixAfterDecompsition :: OneStepPolicy a -> Decomposition a  -> OneStepPolicy a
intermixAfterDecompsition q (Left x)        = Atomic x <> q
intermixAfterDecompsition q (Right (x, xs)) = Atomic x <> (q <||> xs)

sequenceAfterDecomposition :: OneStepPolicy a -> Decomposition a  -> Decomposition a
sequenceAfterDecomposition q (Left x)        = Right (x, q)
sequenceAfterDecomposition q (Right (x, xs)) = Right (x, Sequence xs q)

newtype PartialNDEndo a = PartialNDEndo
    { applyPartialNDEndo :: a -> Set (Partial a)
    }

instance (Ord a, Monoid a) => Semigroup (PartialNDEndo a) where
    (PartialNDEndo f) <> (PartialNDEndo g) = PartialNDEndo $ \x -> Set.fromList
        [ pb <> chooseAll (chosen pa) | pa <- toList $ f x, pb <- toList $ g (rest pa) ]

instance (Ord a, Monoid a) => Monoid (PartialNDEndo a) where
    mempty = PartialNDEndo $ Set.singleton . chooseNoneOf


newtype OneStep t = OneStep
    { executeOneStep :: PartialNDEndo (History t)
    } deriving newtype (Semigroup)

instance Show1 OneStep where
  liftShowsPrec _ _ _ _ = shows "OneStep [\\h -> ..]"

oneStepChoice :: Ord t => OneStep t -> OneStep t -> OneStep t
oneStepChoice (OneStep p) (OneStep q) = OneStep . PartialNDEndo $
    \h -> applyPartialNDEndo p h <> applyPartialNDEndo q h

instance Ord t => CreatesBellPairs (OneStep t) t where
    tryCreateBellPairFrom (CreateBellPairArgs pt bp bps prob t dk) =
        OneStep . PartialNDEndo $ \h@(History ts) ->
            case findTreeRootsNDP bellPair bps (bellPairTag >$< pt) ts of
            [] -> [chooseNoneOf h]
            partialNewTs ->
                mconcat
                [ case prob of
                    Nothing -> [ History <$> mapChosen (Mset.singleton . processDup dk (TaggedBellPair bp t)) partial ]
                    Just _  -> [ History <$> mapChosen (Mset.singleton . processDup dk (TaggedBellPair bp t)) partial
                                , History <$> partial { chosen = [] }
                                ]
                | partial <- partialNewTs
                ]

instance Ord t => Tests (OneStep t) t where
  test p = OneStep . PartialNDEndo $ \h@(History ts) ->
    if p (Mset.map rootLabel ts) then [ chooseNoneOf h ] else []

data OneStepFree t = OSFCreate (CreateBellPairArgs t) | OSFTest

instance Show1 OneStepFree where
  liftShowsPrec _ _ _ (OSFCreate _) = showString "create(" . showString ")"
  liftShowsPrec _ _ _ OSFTest = showString "[..]"

instance CreatesBellPairs (OneStepFree t) t where
  tryCreateBellPairFrom = OSFCreate

instance Tests (OneStepFree t) t where
  test _ = OSFTest

instance CreatesBellPairs a t =>  CreatesBellPairs (OneStepPolicy a) t where
    tryCreateBellPairFrom = Atomic . tryCreateBellPairFrom

instance (Ord t) => Quantum (OneStepPolicy (OneStep t)) t

instance (Ord t, Show t, Tests a t) => Tests (OneStepPolicy a) t where
  test = Atomic . test

instance (Ord t, Show t) => TestsQuantum (OneStepPolicy (OneStep t)) t where

instance {-# OVERLAPPING #-} (Show1 f, Show a) => Show (Compose OneStepPolicy f a) where
    showsPrec d  (Compose x) = liftShowsPrec (liftShowsPrec showsPrec showList) (liftShowList showsPrec showList) d x

instance (Ord t) => ParallelSemigroup (Compose OneStepPolicy a t) where
  p <||> q = Compose $ getCompose p <||> getCompose q

instance (Ord t) => ChoiceSemigroup (Compose OneStepPolicy a t) where
  p <+> q = Compose $ getCompose p <||> getCompose q

instance (Ord t) => OrderedSemigroup (Compose OneStepPolicy a t) where
  p <.> q = Compose $ getCompose p <> getCompose q

instance (Ord t, CreatesBellPairs (a t) t) => CreatesBellPairs (Compose OneStepPolicy a t) t where
  tryCreateBellPairFrom = Compose . tryCreateBellPairFrom

instance (Ord t, CreatesBellPairs (a t) t) => Quantum (Compose OneStepPolicy a t) t where

instance (Show t, Ord t, Tests (a t) t) => Tests (Compose OneStepPolicy a t) t where
  test = Compose . test

instance (Show t, Ord t, Tests (a t) t, CreatesBellPairs (a t) t) => TestsQuantum (Compose OneStepPolicy a t) t where

executeOneStepPolicy :: (Ord t) => OneStepPolicy (OneStep t) -> OneStep t
executeOneStepPolicy (Atomic x) = x
executeOneStepPolicy (Sequence p q)  = executeOneStepPolicy p <> executeOneStepPolicy q
executeOneStepPolicy (Choice p q)  = oneStepChoice (executeOneStepPolicy p) (executeOneStepPolicy q)

executePartial :: Ord t => Compose OneStepPolicy OneStep t -> History t -> Set (Partial (History t))
executePartial (Compose osp) = applyPartialNDEndo (executeOneStep (executeOneStepPolicy osp))

execute :: Ord t => Compose OneStepPolicy OneStep t -> History t -> Set (History t)
execute p = Set.map unchoose . executePartial p
