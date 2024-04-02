{-# LANGUAGE OverloadedStrings #-}

import BellKAT.Prelude

p :: BellKATPolicy 
p =
    (create "C" <||> create "C" <||> create "C" <||> create "C") 
    <>
    (trans "C" ("C", "A") <||> trans "C" ("C", "B") <||> trans "C" ("C", "D") <||> trans "C" ("C", "D") <||> create "E" <||> create "E") 
    <>
    (swap "C" ("A", "D") <||> swap "C" ("B", "D") <||> trans "E" ("E", "D") <||> trans "E" ("E", "D"))
    <> 
    (swap "D" ("A", "E") <||> swap "D" ("B", "E"))


main :: IO ()
main = drawHistoriesText p
