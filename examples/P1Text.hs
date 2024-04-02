{-# LANGUAGE OverloadedStrings #-}

import BellKAT.Prelude

p :: BellKATPolicy 
p =
    (create "C" <||> create "C" <||> create "E" <||> create "E") 
    <>
    (trans "C" ("A", "D") <||> trans "C" ("B", "D") <||> trans "E" ("E", "D") <||> trans "E" ("E", "D")) 
    <>
    (swap "D" ("A", "E") <||> swap "D" ("B", "E"))


main :: IO ()
main = drawHistoriesText p
