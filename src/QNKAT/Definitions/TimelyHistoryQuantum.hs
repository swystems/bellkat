{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData      #-}
module QNKAT.Definitions.TimelyHistoryQuantum (execute) where

import           Data.Foldable              (toList)
import           Data.Functor.Contravariant ((>$<))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set

import           Data.Vector.Fixed          (pattern V2)

import           QNKAT.ChoiceUtilities
import           QNKAT.Definitions.Core
import           QNKAT.UnorderedTree

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
                | (V2 hs hs', hRest) <- toList $
                    chooseKHistories (V2 (requiredRootsTimely hq) (requiredRootsTimely hq')) h
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

execute :: Ord t => TimelyHistoryQuantum t -> History t -> Set (History t)
execute h = Set.map fst . executeTimely h