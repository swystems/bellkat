{-# LANGUAGE OverloadedStrings #-}

import Diagrams.Backend.Cairo.CmdLine

import BellKAT.DSL
import BellKAT.Drawing
import BellKAT.Definitions hiding (test, (<.>))

type PaperPolicy = NormalWithTests StarPolicy FreeTest (Maybe ())

p :: PaperPolicy 
p =
    (create "C" <||> create "C" <||> create "E" <||> create "E") 
    <>
    (trans "C" ("A", "D") <||> trans "C" ("B", "D") <||> trans "E" ("E", "D") <||> trans "E" ("E", "D")) 
    <>
    (swap "D" ("A", "E") <||> swap "D" ("B", "E"))


main :: IO ()
main = mainWith $ drawStarPolicySteps p
