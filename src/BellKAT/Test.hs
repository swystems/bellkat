{-# OPTIONS_GHC -Wno-missing-signatures #-}

module BellKAT.Test (memoryBounds) where

import           GHC.Exts (toList)
import           Data.Map.Strict (Map)
import           BellKAT.Definitions

countQubitsAtLocation :: Ord tag => Location -> TaggedBellPairs tag -> Int
countQubitsAtLocation l = length . filter (hasLocation l . bellPair) . toList

-- | produces a predicate over valid states from upper bounds on the number of qubits at certain
-- locations. The state is valid if and only if /all/ the bounds are satisfied.
--
-- For instance no constraints mean \"always true\" predicate:
--
--  @'memoryBounds' [] = 'const' True@
--
-- To impose an upper bound @3@ on the number of qubits at location @\"A\"@ use (assuming
-- 'OverloadedLists' and 'OverloadedStrings' extensions:
--
--  @'memoryBounds' [(\"A\", 3)]@
memoryBounds 
    :: Ord tag 
    => Map Location Int -- ^ Upper bounds on the number of qubits at respective locations
    -> TaggedBellPairs tag -> Bool
memoryBounds bounds bps = all (\(l,k) -> countQubitsAtLocation l bps <= k) $ toList bounds
