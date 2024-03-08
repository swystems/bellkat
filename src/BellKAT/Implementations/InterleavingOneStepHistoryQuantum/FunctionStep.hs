{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE DataKinds      #-}

module BellKAT.Implementations.InterleavingOneStepHistoryQuantum.FunctionStep
    ( FunctionStep (..)
    , PartialNDEndo (..)
    , FunctionDup (..)
    ) where

import           Data.Kind
import           Data.Functor.Classes
import           Data.Functor.Contravariant   ((>$<))
import qualified Data.Multiset                as Mset

import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Structures
import           BellKAT.Utils.Choice
import           BellKAT.Utils.PartialNDEndo
import           BellKAT.Utils.UnorderedTree    (UTree (..))

data FunctionDup = FDUse | FDTimely 

newtype FunctionStep (dup :: FunctionDup) (test :: Type -> Type) tag = FunctionStep
    { executeFunctionStep :: PartialNDEndo (History tag)
    } deriving newtype (Semigroup)

instance Show1 (FunctionStep dup test) where
  liftShowsPrec _ _ _ _ = shows "FunctionStep [\\h -> ..]"

instance Ord tag => ChoiceSemigroup (FunctionStep dup test tag) where
    (FunctionStep p) <+> (FunctionStep q) = FunctionStep . PartialNDEndo $
        \h -> applyPartialNDEndo p h <> applyPartialNDEndo q h

instance (Ord tag) => CreatesBellPairs (FunctionStep 'FDUse test tag) tag where
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

instance (Ord tag) => CreatesBellPairs (FunctionStep 'FDTimely test tag) tag where
    tryCreateBellPairFrom (CreateBellPairArgs pt bp bps prob t _dk) =
        FunctionStep . PartialNDEndo $ \h@(History ts) ->
            case findTreeRootsNDP bellPair bps (bellPairTag >$< pt) ts of
            [] -> [chooseNoneOf h]
            partialNewTs ->
                mconcat
                [ case prob of
                    Nothing -> [ History <$> mapChosen (Mset.singleton . Node (TaggedBellPair bp t)) partial ]
                    Just _  -> [ History <$> mapChosen (Mset.singleton . Node (TaggedBellPair bp t)) partial
                                , History <$> partial { chosen = [] }
                                ]
                | partial <- partialNewTs
                ]

instance (Ord tag, Test test) => Tests (FunctionStep dup test tag) test tag where
  test t = FunctionStep . PartialNDEndo $ \h@(History ts) ->
    if getBPsPredicate (toBPsPredicate t) (Mset.map rootLabel ts) then [ chooseNoneOf h ] else []
