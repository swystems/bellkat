{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec where

import QNKAT.Definitions
import QNKAT.UnorderedTree
import QNKAT.Test
import QNKAT.Drawing

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid (Sum(..))
import Control.Monad (unless)
import GHC.Exts

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (expectFailure, (===))

main :: IO ()
main = hspec $ do
    describe "distill" $ do
        it "should drop sometimes" $
            applyPolicy (Distill ("A", "B")) [Node ("A" :~: "B") [], Node ("A" :~: "B") []] 
                `historiesShouldSatisfy` any (null . getForest)
    describe "transmit" $ do
        it "should transmit" $
            applyPolicy (Transmit "A" ("A", "R[AB]")) [Node ("A" :~: "A") []]
                `historiesShouldSatisfy` any (any (hasRoot ("A" :~: "R[AB]")) . getForest)
        it "should transmit two" $
            applyPolicy 
                    (Transmit "A" ("A", "R[AB]") <||> Transmit "B" ("B", "R[AB]")) 
                    [Node ("A" :~: "A") [], Node ("B" :~: "B") []]
                `historiesShouldSatisfy` any (any (hasRoot ("A" :~: "R[AB]")) . getForest)
        it "should transmit one out of two" $
            applyPolicy
                   (Transmit "A" ("A", "R[AB]")) 
                    [Node ("A" :~: "A") [], Node ("A" :~: "A") []]
                `historiesShouldSatisfy` all ((== 2) . length . getForest)
        it "should transmit two out of three" $
            applyPolicy
                   (Transmit "A" ("A", "R[AB]") <||> Transmit "A" ("A", "R[AB]")) 
                   (fromList . replicate 3 $ Node ("A" :~: "A") [])
                `historiesShouldSatisfy` all ((== 3) . length . getForest)
    describe "choose" $ do
        it "should choose nothing" $
            choose 0 ["A"] `shouldBe` [chooseNoneOf ["A"]]
        it "should choose one" $
            choose 1 ["A"] `shouldBe` [chooseAll ["A"]]
        it "should choose two" $
            choose 2 ["A" ,"B"] `shouldBe` [chooseAll ["A", "B"]]
        prop "should preserve length" $
            \n (xs :: [Int]) -> all (isPartial (Sum $ length xs) . fmap (Sum . length)) (choose n xs)
    describe "bell pair" $ do
        it "A~B == B~A" $
            ("A" :~: "B") `shouldBe` ("B" :~: "A")
        it "A~B /= B~C" $
            ("A" :~: "B") `shouldNotBe` ("B" :~: "C")
    describe "parallel" $ do 
        prop "should be commutative" parallelCompositionIsCommutative
        prop "should be associative" parallelCompositionIsAssociative
    describe "sequential" $ do
        prop "should be associative" sequentialCompositionIsAssociative
    describe "chooseTwoHistories" $ do
        prop "should be \"commutative\"" $
            \x y h -> chooseTwoHistories x y h === Set.map (\(x, y, z) -> (y, x, z)) (chooseTwoHistories y x h)
    describe "findSubHistoryND" $ do
        prop "should return partial" $ 
            \ps h -> all (isPartial h) (findSubHistoryND ps h)
    describe "findTreeRootsND" $ do
        prop "should return partial" $ 
            \ps h -> all (isPartial h) (findTreeRootsND ps h)

isPartial :: (Eq a, Semigroup a) => a -> Partial a -> Bool
isPartial x partialX = x == (chosen partialX <> rest partialX)

historiesShouldSatisfy :: Set History -> (Set History -> Bool) -> Expectation
historiesShouldSatisfy h f = 
    unless (f h) $ expectationFailure $ "nope:\n" <> drawHistoriesText h

