module BellKAT.Utils.PartialNDEndo
    ( PartialNDEndo (..)
    ) where

import           Data.Foldable                (toList)
import           Data.Set                     (Set)
import qualified Data.Set                     as Set

import           BellKAT.Utils.Choice

newtype PartialNDEndo a = PartialNDEndo
    { applyPartialNDEndo :: a -> Set (Partial a)
    }

instance (Ord a, Monoid a) => Semigroup (PartialNDEndo a) where
    (PartialNDEndo f) <> (PartialNDEndo g) = PartialNDEndo $ \x -> Set.fromList
        [ pb <> chooseAll (chosen pa) | pa <- toList $ f x, pb <- toList $ g (rest pa) ]

instance (Ord a, Monoid a) => Monoid (PartialNDEndo a) where
    mempty = PartialNDEndo $ Set.singleton . chooseNoneOf
