module BellKAT.Utils.NonEmpty where

import           Data.List.NonEmpty                      (NonEmpty (..))

foldNonEmpty :: (a -> a -> a) -> NonEmpty a -> a
foldNonEmpty _ (x :| [])        = x
foldNonEmpty f (x :| (x' : xs)) = let y = f x x' in seq y (foldNonEmpty f (y :| xs))
