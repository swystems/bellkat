{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE UndecidableInstances #-}
module QNKAT.Definitions.OneStepHistoryQuantum (OneStepPolicy(..), execute) where

import           Data.Foldable              (toList)
import           Data.Functor.Contravariant ((>$<))
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.Multiset              as Mset
import           Data.Set                   (Set)
import qualified Data.Set                   as Set

import           Data.Functor.Compose       (Compose (..))
import           QNKAT.ChoiceUtilities
import           QNKAT.Definitions.Core

data OneStepPolicy a
    = Atomic a
    | Sequence (OneStepPolicy a) (OneStepPolicy a)
    | Choice (OneStepPolicy a) (OneStepPolicy a)
    deriving stock (Show)

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

oneStepChoice :: Ord t => OneStep t -> OneStep t -> OneStep t
oneStepChoice (OneStep p) (OneStep q) = OneStep . PartialNDEndo $
    \h -> applyPartialNDEndo p h <> applyPartialNDEndo q h

instance (Ord t) => Quantum (OneStepPolicy (OneStep t)) t where
    tryCreateBellPairFrom (CreateBellPairArgs pt bp bps prob t dk) =
        Atomic . OneStep . PartialNDEndo $ \h@(History ts) ->
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

instance Semigroup (Compose OneStepPolicy OneStep t) where
  p <> q = Compose $ getCompose p <> getCompose q

instance (Ord t) => ParallelSemigroup (Compose OneStepPolicy OneStep t) where
  p <||> q = Compose $ getCompose p <||> getCompose q

instance (Ord t) => Quantum (Compose OneStepPolicy OneStep t) t where
  tryCreateBellPairFrom = Compose . tryCreateBellPairFrom

executeOneStepPolicy :: (Ord t) => OneStepPolicy (OneStep t) -> OneStep t
executeOneStepPolicy (Atomic x) = x
executeOneStepPolicy (Sequence p q)  = executeOneStepPolicy p <> executeOneStepPolicy q
executeOneStepPolicy (Choice p q)  = oneStepChoice (executeOneStepPolicy p) (executeOneStepPolicy q)

execute :: Ord t => Compose OneStepPolicy OneStep t -> History t -> Set (History t)
execute (Compose osp) =
    Set.map unchoose . applyPartialNDEndo (executeOneStep (executeOneStepPolicy osp))
