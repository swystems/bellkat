{-# LANGUAGE FlexibleContexts #-}

module QNKAT.Drawing where

import IHaskell.Display.Diagrams

import Diagrams.Prelude
import Diagrams.Backend.Cairo (B)

import Data.Tree (subForest, rootLabel)

import QNKAT.Definitions

pairToDiagram bp = (text (show bp) <> rect 4 1) # fontSize (local 0.5) 

treeToDiagram t = 
    let childrenNames = [1..(length $ subForest t :: Int)]
        rootName = 0 :: Int
        subtrees = zipWith (.>>) childrenNames (map treeToDiagram $ subForest t)
      in vsep 1 [pairToDiagram (rootLabel t) # named rootName, hsep 0.5 subtrees # centerX  ] 
          # appEndo (mconcat $ map (\i -> Endo $ connectOutside (i .> rootName) rootName) childrenNames)

historyToDiagram (History ts) = hsep 0.5 (map treeToDiagram ts)

drawPolicy :: Policy -> ManuallySized (Diagram B)
drawPolicy p = withImgWidth 600 . vsep 1 . fmap historyToDiagram . applyPolicy p $ History [] 
