{-# LANGUAGE OverloadedLists      #-}

module BellKAT.Implementations.OneStepHistoryQuantum.FunctionStep
    ( FunctionStep (..)
    , PartialNDEndo (..)
    ) where

import           Data.Foldable                (toList)
import           Data.Functor.Classes
import           Data.Functor.Contravariant   ((>$<))
import qualified Data.Multiset                as Mset
import           Data.Set                     (Set)
import qualified Data.Set                     as Set

import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Structures
import           BellKAT.Utils.Choice
import           BellKAT.Utils.UnorderedTree    (UTree (..))

newtype PartialNDEndo a = PartialNDEndo
    { applyPartialNDEndo :: a -> Set (Partial a)
    }

instance (Ord a, Monoid a) => Semigroup (PartialNDEndo a) where
    (PartialNDEndo f) <> (PartialNDEndo g) = PartialNDEndo $ \x -> Set.fromList
        [ pb <> chooseAll (chosen pa) | pa <- toList $ f x, pb <- toList $ g (rest pa) ]

instance (Ord a, Monoid a) => Monoid (PartialNDEndo a) where
    mempty = PartialNDEndo $ Set.singleton . chooseNoneOf

newtype FunctionStep tag = FunctionStep
    { executeFunctionStep :: PartialNDEndo (History tag)
    } deriving newtype (Semigroup)

instance Show1 FunctionStep where
  liftShowsPrec _ _ _ _ = shows "FunctionStep [\\h -> ..]"

instance Ord tag => ChoiceSemigroup (FunctionStep tag) where
    (FunctionStep p) <+> (FunctionStep q) = FunctionStep . PartialNDEndo $
        \h -> applyPartialNDEndo p h <> applyPartialNDEndo q h

instance Ord tag => CreatesBellPairs (FunctionStep tag) tag where
    tryCreateBellPairFrom (CreateBellPairArgs pt bp bps prob t dk) =
        FunctionStep . PartialNDEndo $ \h@(History ts) ->
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

instance Ord tag => Tests (FunctionStep tag) BellPairsPredicate tag where
  test t = FunctionStep . PartialNDEndo $ \h@(History ts) ->
    if getBPsPredicate t (Mset.map rootLabel ts) then [ chooseNoneOf h ] else []
