{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}

module QNKAT.Drawing where

import IHaskell.Display.Diagrams

import Diagrams.Prelude
import Diagrams.Backend.Cairo (B)

import Data.Tree (subForest, rootLabel, drawForest)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (intercalate, intersperse)

import QNKAT.UnorderedTree (toTree, toForest) 
import QNKAT.Definitions

pairToDiagram bp = (text (show bp) <> rect 4 1) # fontSize (local 0.5) 

treeToDiagram t = 
    let childrenNames = [1..(length $ subForest t :: Int)]
        rootName = 0 :: Int
        subtrees = zipWith (.>>) childrenNames (map treeToDiagram $ subForest t)
        drawEdge = connectOutside' (with & lengths .~ global 0.5)
      in vsep 1 [pairToDiagram (rootLabel t) # named rootName, hsep 0.5 subtrees # centerX] 
          # appEndo (mconcat $ map (\i -> Endo $ drawEdge  (i .> rootName) rootName) childrenNames)

frameDiagram d = let d' = d # frame 0.5 in d' <> boundingRect d'

historyToDiagram (History ts) = hsep 0.5 . map treeToDiagram . toForest $ ts

historiesToDiagram = vsep 1 . fmap (alignL . frameDiagram . historyToDiagram)

drawPolicy :: Policy -> ManuallySized (Diagram B)
drawPolicy p = withImgWidth 600 . historiesToDiagram . Set.elems . applyPolicy p $ []

drawHistoryText :: History -> String
drawHistoryText = drawForest . (fmap . fmap) show . toForest . getForest

drawHistoriesText :: Set History -> String
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
