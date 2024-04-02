{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module PreludeSpec (spec) where

import BellKAT.Prelude
import Test.Hspec

p :: BellKATPolicy
p = create "A"

q :: BellKATPolicy
q = create "B" <> (trans "B" ("A", "A") <||> trans "A" ("B", "B"))

spec :: Spec
spec = do
    describe "isPolicyValid" $ do
        it "correctly checks create when not enough" $
            isPolicyValid [[]] (memoryBounds [("A", 1)]) p `shouldBe` False
        it "correctly checks create when enough" $
            isPolicyValid [[]] (memoryBounds [("A", 2)]) p `shouldBe` True
        it "correctly checks create when non-empty init" $
            isPolicyValid [["A" ~ "B"]] (memoryBounds [("A", 2)]) p `shouldBe` False
    describe "arePoliciesEquivalent" $ do
        it "correctly verifies that 'p' and 'q' are equivalent on empty state" $
            arePoliciesEquivalent [[]] p q `shouldBe` True
        it "correctly verifies that 'p' and 'q' are _not_ equivalent when starting withy A~A" $
            arePoliciesEquivalent [["A" ~ "A"]] p q `shouldBe` False
