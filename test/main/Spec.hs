{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Spec where

import           BellKAT.Utils.Choice
import           BellKAT.Definitions
import           BellKAT.Drawing
import           BellKAT.DSL
import           BellKAT.Test
import           BellKAT.Utils.UnorderedTree

import           Control.Monad              (unless)
import           Data.Functor.Contravariant (Predicate (..))
import           Data.Monoid                (Sum (..))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Vector.Fixed          (mk2)
import qualified Data.Vector.Fixed          as FV
import           GHC.Exts

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck            ((===), mapSize)

main :: IO ()
main = hspec $ do
    describe "distill" $ do
        it "should drop sometimes" $
            applyPolicy @Tag (distill ("A", "B")) [node ("A" :~: "B"), node ("A" :~: "B")]
                `historiesShouldSatisfy` any (null . getForest)
    describe "transmit" $ do
        it "should transmit" $
            applyPolicy @Tag (trans "A" ("A", "R[AB]")) [node ("A" :~: "A")]
                `historiesShouldSatisfy` all (all (hasBellPair ("A" :~: "R[AB]")) . getForest)
        it "should transmit two" $
            applyPolicy @Tag
                    (trans "A" ("A", "R[AB]") <||> trans "B" ("B", "R[AB]"))
                    [node ("A" :~: "A"), node ("B" :~: "B")]
                `historiesShouldSatisfy` any (any (hasBellPair ("A" :~: "R[AB]")) . getForest)
        it "should transmit both" $
            applyPolicy @Tag
                    (trans "A" ("A", "B") <||> trans "A" ("A", "B"))
                    [node ("A" :~: "A"), node ("A" :~: "A")]
                `historiesShouldSatisfy` all (all (hasBellPair ("A" :~: "B")) . getForest)
        it "should transmit one out of two" $
            applyPolicy @Tag
                   (trans "A" ("A", "R[AB]"))
                    [node ("A" :~: "A"), node ("A" :~: "A")]
                `historiesShouldSatisfy` all ((== 2) . length . getForest)
        it "should transmit two out of three" $
            applyPolicy @Tag
                   (trans "A" ("A", "R[AB]") <||> trans "A" ("A", "R[AB]"))
                   (fromList . replicate 3 $ node ("A" :~: "A"))
                `historiesShouldSatisfy` all ((== 3) . length . getForest)
        it "should not transmit if wrong tag" $
            applyPolicy @Tag (tags [1] ?~ trans "A" ("A", "B"))
                    [node ("A" :~: "A") .~ 2]
                `historiesShouldSatisfy` all (all (hasBellPair ("A" :~: "A")) . getForest)
        it "should not transmit if wrong tag but should if the right" $
            applyPolicy @Tag
                (tags [1] ?~ trans "A" ("A", "B") <||> tags [1] ?~ trans "A" ("A", "B"))
                [node ("A" :~: "A") .~ 2, node ("A" :~: "A") .~ 1]
                `historiesShouldSatisfy` all (any (hasBellPair ("A" :~: "A")) . getForest)
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
        prop "should be commutative" $ mapSize (const 1) parallelCompositionIsCommutative
        prop "should be associative" $ mapSize (const 1) parallelCompositionIsAssociative
    describe "sequential" $ do
        prop "should be associative" sequentialCompositionIsAssociative
    describe "parallel (timely)" $ do
        prop "should be commutative" $ mapSize (const 1) timelyParallelCompositionIsCommutative
        prop "should be associative" $ mapSize (const 1) timelyParallelCompositionIsAssociative
    describe "sequential (timely)" $ do
        prop "should be associative" timelySequentialCompositionIsAssociative
    describe "sequential (steps)" $ do
        prop "should be associative" stepsSequentialCompositionIsAssociative
    describe "parallel (steps)" $ do
        prop "should be commutative" $ mapSize (const 1) stepsParallelCompositionIsCommutative
        prop "should be associative" $ mapSize (const 1) stepsParallelCompositionIsAssociative
    describe "chooseKHistories" $ do
        prop "should be \"commutative\"" $
            let setToPredicate (x, y) = (x, Predicate (`Set.member` y))
                swapTuples = Set.map (\(xs, x3) -> (FV.reverse xs, x3))
             in \x y h ->
                 chooseKHistories @Tag (mk2 (setToPredicate <$> x) (setToPredicate <$> y)) h
                    === swapTuples  (chooseKHistories @Tag (mk2 (setToPredicate <$> y) (setToPredicate <$> x)) h)
    describe "findTreeRootsND" $ do
        prop "should return partial" $
            \ps (h :: UForest BellPair) -> all (isPartial h) (findTreeRootsND ps h)

tags :: [Int] -> [Int]
tags = id

hasBellPair :: BellPair -> UTree (TaggedBellPair a) -> Bool
hasBellPair bp = (== bp) . bellPair . rootLabel

isPartial :: (Eq a, Semigroup a) => a -> Partial a -> Bool
isPartial x partialX = x == (chosen partialX <> rest partialX)

historiesShouldSatisfy :: Show t => Set (History t) -> (Set (History t) -> Bool) -> Expectation
historiesShouldSatisfy h f =
    unless (f h) $ expectationFailure $ "nope:\n" <> drawHistoriesText h
