{-# LANGUAGE OverloadedLists      #-}

module BellKAT.Implementations.AtomicOneStepHistoryQuantum
    ( AtomicOneStepPolicy
    , execute
    ) where

import           Data.List.NonEmpty             (NonEmpty(..))
import           Data.Foldable              (toList)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Default
import qualified Data.Multiset                as Mset

import BellKAT.Definitions.Core
import BellKAT.Definitions.Structures
import BellKAT.Utils.Choice

data AtomicOneStepPolicy tag = AtomicOneStepPolicy
    (RestrictedTest tag) (TaggedBellPairs tag) (TaggedBellPairs tag)
    deriving stock (Eq, Ord)

createAtomicOneStepPolicy 
    :: Ord tag
    => RestrictedTest tag -> TaggedBellPairs tag -> TaggedBellPairs tag -> AtomicOneStepPolicy tag
createAtomicOneStepPolicy t inBPs outBPs = 
    if t == createRestrictedTest [mempty]
       then AtomicOneStepPolicy t mempty mempty
       else AtomicOneStepPolicy t inBPs outBPs

instance Show tag => Show (AtomicOneStepPolicy tag) where
    showsPrec _ (AtomicOneStepPolicy t inBPs outBPs) = 
        showString "[" . shows t . showString "] (" 
        . shows (toList inBPs) . showString "|>" . shows (toList outBPs)
        . showString ")"

execute
    :: (Ord tag)
    => AtomicOneStepPolicy tag
    -> TaggedBellPairs tag
    -> Set (TaggedBellPairs tag)
execute (AtomicOneStepPolicy t inBPs outBPs) bps =
    if getBPsPredicate (toBPsPredicate t) bps
       then Set.fromList [ outBPs <> rest partial  | partial <- findElemsND (toList inBPs) bps]
       else mempty

instance Ord tag => OrderedSemigroup (AtomicOneStepPolicy tag) where
    (AtomicOneStepPolicy t1 inBps1 outBps1) <.> (AtomicOneStepPolicy t2 inBps2 outBps2)
      = createAtomicOneStepPolicy (t1 .&&. (t2 .+. inBps1)) (inBps1 <> inBps2) (outBps1 <> outBps2)

instance Ord tag => ParallelSemigroup (AtomicOneStepPolicy tag) where
    (AtomicOneStepPolicy t1 inBps1 outBps1) <||> (AtomicOneStepPolicy t2 inBps2 outBps2)
      = createAtomicOneStepPolicy ((t1 .+. inBps2) .&&. (t2 .+. inBps1)) (inBps1 <> inBps2) (outBps1 <> outBps2)

instance (Ord tag, Default tag) 
  => CreatesBellPairs (NonEmpty (AtomicOneStepPolicy tag)) tag where
    tryCreateBellPairFrom (CreateBellPairArgs _ bp bps prob t _) =
        case prob of
          Nothing -> 
            createBasicAction
                (Mset.fromList $ (`TaggedBellPair` def) <$>  bps) [TaggedBellPair bp t]
          Just _ ->
              createBasicAction
                (Mset.fromList $ (`TaggedBellPair` def) <$> bps) [TaggedBellPair bp t]
              <> createBasicAction (Mset.fromList $ (`TaggedBellPair` def) <$> bps) []

instance Ord tag => Tests (AtomicOneStepPolicy tag) FreeTest tag where
    test t = 
        let (s, sig) = getSetAndSign t
         in if sig then
                AtomicOneStepPolicy (createRestrictedTest mempty) s s
            else
                AtomicOneStepPolicy (createRestrictedTest [s]) mempty mempty

getSetAndSign :: FreeTest tag -> (TaggedBellPairs tag, Bool)
getSetAndSign (FTSubset s) = (s, True)
getSetAndSign (FTNot t) = let (s, sig) = getSetAndSign t in (s, not sig)

createBasicAction 
    :: (Ord tag) 
    => TaggedBellPairs tag -> TaggedBellPairs tag -> NonEmpty (AtomicOneStepPolicy tag)
createBasicAction inBPs outBPs =
    createAtomicOneStepPolicy (createRestrictedTest mempty) inBPs outBPs
    :| createAtomicOneStepPolicy (createRestrictedTest [inBPs]) mempty mempty
    : []
