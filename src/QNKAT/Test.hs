module QNKAT.Test where

import Test.QuickCheck

import Data.List (intercalate, intersperse)
import qualified Data.Set as Set

import QNKAT.Definitions
import QNKAT.Drawing
import Debug.Trace

(~) :: Policy -> Policy -> History -> Property
(~) p q h =
    let hsP = applyPolicy p h
        hsQ = applyPolicy q h

        counterexampleText = intercalate "\n" $ 
               [drawHistoriesText hsP]
            <> ["   =/=   "]
            <> [drawHistoriesText hsQ]
     in counterexample counterexampleText (hsP == hsQ)

sequentialCompositionIsAssociative p q s = ((p <> q) <> s) ~ (p <> (q <> s))
parallelCompositionIsAssociative p q s = ((p <||> q) <||> s) ~ (p <||> (q <||> s))
parallelCompositionIsCommutative p q = (p <||> q) ~ (q <||> p)
sequentialCompositionDistributes p q s = ((p <||> q) <> s) ~ ((p <> s) <||> (q <> s))


qnkatQuickCheck :: Testable prop => prop -> IO ()
qnkatQuickCheck = quickCheckWith (stdArgs { maxSize = 4, maxSuccess = 10000 })
