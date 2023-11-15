{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData      #-}

module BellKAT.Implementations.HistoryQuantum (HistoryQuantum, execute) where

import           Data.Foldable              (toList)
import           Data.Functor.Contravariant ((>$<))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set

import           Data.Vector.Fixed          (pattern V2)

import           BellKAT.Utils.Choice
import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Structures

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
                    | (V2 hs hs', hRest) <- toList $
                        chooseKHistories (V2 (requiredRoots hq) (requiredRoots hq')) h
                    , hNew <- Set.elems $ execute hq hs
                    , hNew' <- Set.elems $ execute hq' hs'
                ]
        }

instance Ord t => CreatesBellPairs (HistoryQuantum t) t where
    tryCreateBellPairFrom (CreateBellPairArgs pt bp bps prob t dk) = HistoryQuantum
        { requiredRoots = [(bps, pt)]
        , execute = \h@(History ts) ->
            case findTreeRootsNDP bellPair bps (bellPairTag >$< pt) ts of
                [] -> [h]
                partialTsNews ->
                    mconcat
                    [ case prob of
                        Nothing -> [History tsRest <> [processDup dk (TaggedBellPair bp t) tsNew]]
                        Just _ -> [History tsRest <> [processDup dk (TaggedBellPair bp t) tsNew], History tsRest]
                    | Partial { chosen = tsNew, rest = tsRest } <- partialTsNews
                    ]
        }

instance Ord t => Quantum (HistoryQuantum t) t where
