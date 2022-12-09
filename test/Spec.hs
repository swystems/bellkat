{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Spec where

import QNKAT.Definitions
import QNKAT.UnorderedTree
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "distill" $ do
        it "should drop sometimes" $
            applyPolicy (Distill ("A", "B")) [Node ("A" :~: "B") [], Node ("A" :~: "B") []] 
                `shouldSatisfy` any (null . getForest)
    describe "transmit" $ do
        it "should transmit" $
            applyPolicy (Transmit "A" ("A", "R[AB]")) [Node ("A" :~: "A") []]
                `shouldSatisfy` any (any (hasRoot ("A" :~: "R[AB]")) . getForest)
    describe "bell pair" $ do
        it "A~B == B~A" $
            ("A" :~: "B") `shouldBe` ("B" :~: "A")
        it "A~B /= B~C" $
            ("A" :~: "B") `shouldNotBe` ("B" :~: "C")

