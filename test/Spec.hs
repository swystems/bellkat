{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec where

import           QNKAT.ChoiceUtilities
import           QNKAT.Definitions
import           QNKAT.Drawing
import           QNKAT.Test
import           QNKAT.UnorderedTree

import           Control.Monad         (unless)
import           Data.Monoid           (Sum (..))
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           GHC.Exts

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck       ((===))

main :: IO ()
main = hspec $ do
    describe "distill" $ do
        it "should drop sometimes" $
            applyPolicy (Distill ("A", "B")) [Node ("A" :~: "B") [], Node ("A" :~: "B") []]
                `historiesShouldSatisfy` any (null . getForest)
    describe "transmit" $ do
        it "should transmit" $
            applyPolicy (Transmit "A" ("A", "R[AB]")) [Node ("A" :~: "A") []]
                `historiesShouldSatisfy` all (all (hasRoot ("A" :~: "R[AB]")) . getForest)
        it "should transmit two" $
            applyPolicy
                    (Transmit "A" ("A", "R[AB]") <||> Transmit "B" ("B", "R[AB]"))
                    [Node ("A" :~: "A") [], Node ("B" :~: "B") []]
                `historiesShouldSatisfy` any (any (hasRoot ("A" :~: "R[AB]")) . getForest)
        it "should transmit both" $
            applyPolicy
                    (Transmit "A" ("A", "B") <||> Transmit "A" ("A", "B"))
                    [Node ("A" :~: "A") [], Node ("A" :~: "A") []]
                `historiesShouldSatisfy` all (all (hasRoot ("A" :~: "B")) . getForest)
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
            choose 0 (["A"] :: [String]) `shouldBe` [chooseNoneOf ["A"]]
        it "should choose one" $
            choose 1 (["A"] :: [String]) `shouldBe` [chooseAll ["A"]]
        it "should choose two" $
            choose 2 (["A" ,"B"] :: [String]) `shouldBe` [chooseAll ["A", "B"]]
        prop "should preserve length" $
            \n (xs :: [Int]) -> all (isPartial (Sum $ length xs) . fmap (Sum . length)) (choose n xs)
    describe "bell pair" $ do
        it "A~B == B~A" $
            ("A" :~: "B") `shouldBe` ("B" :~: "A")
        it "A~B /= B~C" $
            ("A" :~: "B") `shouldNotBe` ("B" :~: "C")
    describe "chooseTreesND" $ do
        it "should take two if able" $
            chooseTreesND [["A":~:"A"], ["A":~:"A"]] [Node ("A" :~: "A") [], Node ("A" :~: "A") []]
                `shouldBe` [[Just [Node ("A" :~: "A") []], Just [Node ("A" :~: "A") []]]]
    describe "parallel" $ do
        prop "should be commutative" parallelCompositionIsCommutative
        prop "should be associative" parallelCompositionIsAssociative
    describe "sequential" $ do
        prop "should be associative" sequentialCompositionIsAssociative
    describe "parallel (timely)" $ do
        prop "should be commutative" timelyParallelCompositionIsCommutative
        prop "should be associative" timelyParallelCompositionIsAssociative
    describe "sequential (timely)" $ do
        prop "should be associative" timelySequentialCompositionIsAssociative
    describe "sequential (steps)" $ do
        prop "should be associative" stepsSequentialCompositionIsAssociative
    describe "parallel (steps)" $ do
        prop "should be commutative" stepsParallelCompositionIsCommutative
        prop "should be associative" stepsParallelCompositionIsAssociative
    describe "chooseTwoHistories" $ do
        prop "should be \"commutative\"" $
            \x y h -> chooseTwoHistories x y h === Set.map (\(x1, x2, x3) -> (x2, x1, x3)) (chooseTwoHistories y x h)
    describe "findTreeRootsND" $ do
        prop "should return partial" $
            \ps (h :: UForest BellPair) -> all (isPartial h) (findTreeRootsND ps h)

isPartial :: (Eq a, Semigroup a) => a -> Partial a -> Bool
isPartial x partialX = x == (chosen partialX <> rest partialX)

historiesShouldSatisfy :: Set History -> (Set History -> Bool) -> Expectation
historiesShouldSatisfy h f =
    unless (f h) $ expectationFailure $ "nope:\n" <> drawHistoriesText h

