{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Spec where

import QNKAT.Definitions
import QNKAT.UnorderedTree
import Test.Hspec

main :: IO ()
main = hspec $ 
    describe "distill" $ do
        it "should drop sometimes" $
            applyPolicy (Distill ("A", "B")) [Node ("A" :~: "B") [], Node ("A" :~: "B") []] 
                `shouldSatisfy` any (null . getForest)

