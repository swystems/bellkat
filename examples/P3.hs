{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import BellKAT.Prelude
import Test.Hspec

p :: BellKATPolicy
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
            arePoliciesEquivalent [[]] p (p <> test ("A" ~~? "E")) `shouldBe` True
        it "uses more than 1 qubit at A" $
            isPolicyValid [[]] (memoryBounds [("A", 1)]) p `shouldBe` False
        it "uses more than 3 qubit at D" $
            isPolicyValid [[]] (memoryBounds [("D", 3)]) p `shouldBe` False
        it "uses no more than 2 qubits at A and no more than 4 at D" $
            isPolicyValid [[]] (memoryBounds [("A", 2), ("D", 4)]) p `shouldBe` True

