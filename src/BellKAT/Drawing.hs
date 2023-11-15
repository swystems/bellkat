{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}

module BellKAT.Drawing where

import IHaskell.Display.Diagrams

import Diagrams.Prelude
import Diagrams.Backend.Cairo (B)

import Data.Tree (subForest, rootLabel, drawForest)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (intercalate, intersperse)

import BellKAT.Utils.UnorderedTree (toForest) 
import BellKAT.Definitions.Core
import BellKAT.Definitions.Policy
import BellKAT.Definitions

pairToDiagram :: (Show t, Eq t) => TaggedBellPair (Maybe t) -> Diagram B
pairToDiagram (TaggedBellPair bp Nothing) 
  = (text (show bp) <> rect 4 1) # fontSize (local 0.5) 
pairToDiagram (TaggedBellPair bp (Just t)) 
  = (text (show bp <> "[" <> show t <> "]") <> rect 4 1) # fontSize (local 0.5) 

treeToDiagram t = 
    let childrenNames = [1..(length $ subForest t :: Int)]
        rootName = 0 :: Int
        subtrees = zipWith (.>>) childrenNames (map treeToDiagram $ subForest t)
        drawEdge = connectOutside' (with & lengths .~ global 0.5)
      in vsep 1 [pairToDiagram (rootLabel t) # named rootName, hsep 0.5 subtrees # centerX] 
          # appEndo (mconcat $ map (\i -> Endo $ drawEdge  (i .> rootName) rootName) childrenNames)

frameDiagram d = let d' = d # frame 0.5 in d' <> boundingRect d'

historyToDiagram :: (Ord t, Show t) => History (Maybe t) -> Diagram B
historyToDiagram (History []) = rect 4 0
historyToDiagram (History ts) = hsep 0.5 . map treeToDiagram  . toForest $ ts

historiesToDiagram :: (Ord t, Show t) => [History (Maybe t)] -> Diagram B
historiesToDiagram = vsep 1 . fmap (alignL . frameDiagram . historyToDiagram)

drawPolicy :: (Ord t, Show t) => Normal Policy (Maybe t) -> ManuallySized (Diagram B)
drawPolicy p = withImgWidth 600 . historiesToDiagram . Set.elems . applyPolicy p $ []

drawPolicyTimely :: (Ord t, Show t) => Normal Policy (Maybe t) -> ManuallySized (Diagram B)
drawPolicyTimely p = withImgWidth 600 . historiesToDiagram . Set.elems . applyPolicyTimely p $ []

drawPolicySteps :: (Ord t, Show t) => Normal Policy (Maybe t) -> ManuallySized (Diagram B)
drawPolicySteps p = withImgWidth 600 . historiesToDiagram . Set.elems . applyPolicySteps p $ []

drawOrderedPolicySteps :: (Ord t, Show t) => Ordered Policy (Maybe t) -> ManuallySized (Diagram B)
drawOrderedPolicySteps p = withImgWidth 600 . historiesToDiagram . Set.elems . applyOrderedPolicy p $ []

drawFullOrderedPolicySteps :: (Ord t, Show t) => Ordered FullPolicy (Maybe t) -> ManuallySized (Diagram B)
drawFullOrderedPolicySteps p = withImgWidth 600 . historiesToDiagram . Set.elems . applyFullOrderedPolicy p $ []

drawStarOrderedPolicySteps :: (Ord t, Show t) => Ordered StarPolicy (Maybe t) -> ManuallySized (Diagram B)
drawStarOrderedPolicySteps p = withImgWidth 600 . historiesToDiagram . Set.elems . applyStarOrderedPolicy p $ []

drawStarOrderedPolicyStepsBounded :: (Ord t, Show t) => Ordered StarPolicy (Maybe t) -> ManuallySized (Diagram B)
drawStarOrderedPolicyStepsBounded p = withImgWidth 600 . historiesToDiagram . Set.elems . applyStarOrderedPolicyBounded p $ []

drawHistoryText :: Show t => History t -> String
drawHistoryText = drawForest . (fmap . fmap) show . toForest . getForest

drawHistoriesText :: Show t => Set (History t) -> String
drawHistoriesText hs = 
    intercalate "\n" $ 
           ["========="]
        <> intersperse "---------" 
            (map (removeFinalEOL . drawHistoryText) . Set.elems $ hs)
        <> ["========="]


removeFinalEOL :: String -> String
removeFinalEOL x
    | not (null x) && last x == '\n' = removeFinalEOL (init x)
    | otherwise =  x
