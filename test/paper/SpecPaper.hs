{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module SpecPaper where

import Data.Foldable (toList)
import BellKAT.DSL
import BellKAT.Definitions hiding (test, (<.>))

type PaperPolicy = NormalWithTests StarPolicy FreeTest (Maybe ())

main :: IO ()
main =
    let 
        pd :: PaperPolicy  =
            (create "C" <||> create "C") <>
                (trans "C" ("A", "D") <||> trans "C" ("A", "D")) <>
                    distill ("A", "D")
        bpd :: PaperPolicy =
            (test ("A" /~? "D") <.> create "C" <||> test ("A" /~? "D") <.> create "C") <>
                (trans "C" ("A", "D") <||> trans "C" ("A", "D")) <>
                    distill ("A", "D")
        pd' :: PaperPolicy =
            (create "E" <||> create "E") <>
                (trans "E" ("E", "D") <||> trans "E" ("E", "D")) <>
                    distill ("E", "D")
        bpd' :: PaperPolicy =
            (test ("E" /~? "D") <.> create "E" <||> test ("E" /~? "D") <.> create "E") <>
                (trans "E" ("E", "D") <||> trans "E" ("E", "D")) <>
                    distill ("E", "D")
        pad = pd <> star bpd <> test ("A" ~~? "D") 
        ped = pd' <> star bpd' <> test ("E" ~~? "D")
        p =  (pad <||> ped) <> swap "D" ("A", "E")
    in do
       mapM_ (print . toList) $ toList $ applyStarPolicy @(Maybe ()) p []

