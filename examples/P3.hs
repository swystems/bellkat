{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Exts (toList)
import BellKAT.DSL
import BellKAT.Definitions hiding (test, (<.>))
import Data.Map.Strict (Map)
import Test.Hspec

type PaperPolicy = NormalWithTests StarPolicy FreeTest (Maybe ())

countQubitsAtLocation :: Ord tag => Location -> TaggedBellPairs tag -> Int
countQubitsAtLocation l = length . filter (hasLocation l . bellPair) . toList

memoryBounds :: Ord tag => Map Location Int -> TaggedBellPairs tag -> Bool
memoryBounds bounds bps = all (\(l,k) -> countQubitsAtLocation l bps <= k) $ toList bounds

p :: PaperPolicy
p = 
    let 
        pd =
            (create "C" <||> create "C") <>
                (trans "C" ("A", "D") <||> trans "C" ("A", "D")) <>
                    distill ("A", "D")
        bpd =
            (test ("A" /~? "D") <.> create "C" <||> test ("A" /~? "D") <.> create "C") <>
                (trans "C" ("A", "D") <||> trans "C" ("A", "D")) <>
                    distill ("A", "D")
        pd' =
            (create "E" <||> create "E") <>
                (trans "E" ("E", "D") <||> trans "E" ("E", "D")) <>
                    distill ("E", "D")
        bpd' =
            (test ("E" /~? "D") <.> create "E" <||> test ("E" /~? "D") <.> create "E") <>
                (trans "E" ("E", "D") <||> trans "E" ("E", "D")) <>
                    distill ("E", "D")
        pad = pd <> star bpd <> test ("A" ~~? "D") 
        ped = pd' <> star bpd' <> test ("E" ~~? "D")
     in (pad <||> ped) <> swap "D" ("A", "E")

main :: IO ()
main = hspec $ do
    describe "Example (P3)" $ do
        it "always returns A~E" $
            applyStarPolicy p [] `shouldBe` [["A" ~ "E"]]
        it "uses more than 1 qubit at A" $
            applyStarPolicyWithValidity (memoryBounds [("A", 1)]) p [] `shouldBe` Nothing
        it "uses more than 3 qubit at D" $
            applyStarPolicyWithValidity (memoryBounds [("D", 3)]) p [] `shouldBe` Nothing
        it "uses no more than 2 qubits at A and no more than 4 at D" $
            applyStarPolicyWithValidity (memoryBounds [("A", 2), ("D", 4)]) p [] `shouldNotBe` Nothing

