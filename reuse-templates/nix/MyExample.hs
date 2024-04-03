-- OverloadedLists extension allows to write sets and maps using list notation. For instance:
--
--  * [[]] for a set containing an empty multi-set
--
--  * [["A" ~ "B"], ["A" ~ "B", "A" ~ "B"]] for a set containing two multisets of Bell pairs
{-# LANGUAGE OverloadedLists #-}
-- OverloadedStrings extension allows to write string literals (e.g., "A") in place of Location
{-# LANGUAGE OverloadedStrings #-}

import BellKAT.Prelude
-- import testing HSpec testing library
import Test.Hspec

-- | Definition of a policy, creating A~E Bell pair by swapping Bell pairs A~D and D~E at D. The latter
-- two Bell pairs are produced by distillation from two respective copies (i.e., two instance of A~D
-- or A~E)
p :: BellKATPolicy
p = 
    let 
        -- Generate distilled A~D. May fail since distill is probabilistic.
        pd =
            (create "C" <||> create "C") <>
                (trans "C" ("A", "D") <||> trans "C" ("A", "D")) <>
                    distill ("A", "D")
        -- Retry producing a distilled A~D if previous attempt has failed (via `test ("A" /~? "D")`)
        bpd =
            (test ("A" /~? "D") <.> create "C" <||> test ("A" /~? "D") <.> create "C") <>
                (trans "C" ("A", "D") <||> trans "C" ("A", "D")) <>
                    distill ("A", "D")
        -- Generate distilled E~D. May fail since distill is probabilistic.
        pd' =
            (create "E" <||> create "E") <>
                (trans "E" ("E", "D") <||> trans "E" ("E", "D")) <>
                    distill ("E", "D")
        -- Retry producing a distilled E~D if previous attempt has failed (via `test ("E" /~? "D")`)
        bpd' =
            (test ("E" /~? "D") <.> create "E" <||> test ("E" /~? "D") <.> create "E") <>
                (trans "E" ("E", "D") <||> trans "E" ("E", "D")) <>
                    distill ("E", "D")
        -- Encode a `do X while T` construct via `X <> (not T <> X)* <> T`
        pad = pd <> star bpd <> test ("A" ~~? "D") 
        ped = pd' <> star bpd' <> test ("E" ~~? "D")
     in (pad <||> ped) <> swap "D" ("A", "E") -- perform the final swap

-- | Check several properties of the policy `p`, follows the general structure of `Test.Hspec`
-- making use of BellKAT's `arePoliciesEquivalent` and `isPolicyValid` decision procedures.
main :: IO ()
main = hspec $ do
    describe "Example (P3)" $ do
        it "always returns A~E" $
            arePoliciesEquivalent [[]] p (p <> test ("A" ~~? "E")) `shouldBe` True
        it "not always creates A~C" $
            arePoliciesEquivalent [[]] p (p <> test ("A" ~~? "C")) `shouldBe` False
        it "uses more than 1 qubit at A" $
            isPolicyValid [[]] (memoryBounds [("A", 1)]) p `shouldBe` False
        it "uses more than 3 qubit at D" $
            isPolicyValid [[]] (memoryBounds [("D", 3)]) p `shouldBe` False
        it "uses no more than 2 qubits at A and no more than 4 qubits at D" $
            isPolicyValid [[]] (memoryBounds [("A", 2), ("D", 4)]) p `shouldBe` True

