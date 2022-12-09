{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Spec where

import QNKAT.Definitions
import QNKAT.UnorderedTree
import QNKAT.Test

import Test.Hspec
import Test.Hspec.QuickCheck

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
        it "should transmit two" $
            applyPolicy 
                    (Transmit "A" ("A", "R[AB]") <||> Transmit "B" ("B", "R[AB]")) 
                    [Node ("A" :~: "A") [], Node ("B" :~: "B") []]
                `shouldSatisfy` any (any (hasRoot ("A" :~: "R[AB]")) . getForest)
    describe "choose" $ do
        it "should choose nothing" $
            choose 0 ["A"] `shouldBe` [chooseNoneOf ["A"]]
        it "should choose one" $
            choose 1 ["A"] `shouldBe` [chooseAll ["A"]]
        it "should choose two" $
            choose 2 ["A" ,"B"] `shouldBe` [chooseAll ["A", "B"]]
    describe "bell pair" $ do
        it "A~B == B~A" $
            ("A" :~: "B") `shouldBe` ("B" :~: "A")
        it "A~B /= B~C" $
            ("A" :~: "B") `shouldNotBe` ("B" :~: "C")
    describe "parallel" $ do 
        qnkatProp "should be commutative" parallelCompositionIsCommutative
        qnkatProp "should be associative" parallelCompositionIsAssociative
    describe "sequential" $ do
        qnkatProp "should be associative" sequentialCompositionIsAssociative

            
qnkatProp text = modifyMaxSize (const 4) . prop text
