{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module SpecPaper where

import Data.Foldable (toList)
import BellKAT.DSL
import BellKAT.Definitions hiding (test, (<.>))
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

type PaperPolicy = NormalWithTests StarPolicy FreeTest (Maybe ())

countQubitsAtLocation :: Location -> TaggedBellPairs tag -> Int
countQubitsAtLocation l = length . filter (hasLocation l . bellPair) . toList

memoryBounds :: Map Location Int -> TaggedBellPairs tag -> Bool
memoryBounds bounds bps = all (\(l,k) -> countQubitsAtLocation l bps <= k) $ Map.toList bounds

main :: IO ()
main =
    let 
        pd :: PaperPolicy  =
            (create "C" <||> create "C") <>
                (trans "C" ("A", "D") <||> trans "C" ("A", "D")) <>
                    distill ("A", "D")
        bpd :: PaperPolicy =
            (test ("A" /~? "D") <.> create "C" <||> test ("A" /~? "D") <.> create "C") <>
                (trans "C" ("A", "D") <||> trans "C" ("A", "D")) <>
                    distill ("A", "D")
        pd' :: PaperPolicy =
            (create "E" <||> create "E") <>
                (trans "E" ("E", "D") <||> trans "E" ("E", "D")) <>
                    distill ("E", "D")
        bpd' :: PaperPolicy =
            (test ("E" /~? "D") <.> create "E" <||> test ("E" /~? "D") <.> create "E") <>
                (trans "E" ("E", "D") <||> trans "E" ("E", "D")) <>
                    distill ("E", "D")
        pad = pd <> star bpd <> test ("A" ~~? "D") 
        ped = pd' <> star bpd' <> test ("E" ~~? "D")
        p =  (pad <||> ped) <> swap "D" ("A", "E")

        isValidState = memoryBounds (Map.fromList [("A", 2)])
    in do
       mapM_ (print . toList) $ toList $ applyStarPolicy p []
       print $ isJust $ applyStarPolicyWithValidity isValidState p []

