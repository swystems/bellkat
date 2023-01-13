{-# OPTIONS_GHC -Wno-missing-signatures #-}

module QNKAT.Test where

import           Test.QuickCheck

import           Data.List         (intercalate)
import           Data.Set          (Set)

import           QNKAT.Definitions
import           QNKAT.Drawing

testEquality :: (Policy -> History -> Set History) 
             -> Policy -> Policy -> History -> Property
testEquality apply p q h =
    let hsP = apply p h
        hsQ = apply q h

        counterexampleText = intercalate "\n" $
               [drawHistoriesText hsP]
            <> ["   =/=   "]
            <> [drawHistoriesText hsQ]
     in counterexample counterexampleText (hsP == hsQ)


(~) :: Policy -> Policy -> History -> Property
(~) = testEquality applyPolicy

isAssociative :: (a -> a -> p) -> (a -> a -> a) -> a -> a -> a -> p
isAssociative (-~-) (-*-) p q s = ((p -*- q) -*- s) -~- (p -*- (q -*- s))

isCommutative :: (a -> a -> p) -> (a -> a -> a) -> a -> a -> p
isCommutative (-~-) (-*-) p q = (q -*- p) -~- (p -*- q)

distributesOver :: (a -> a -> p) -> (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> p
distributesOver (-~-) (-*-) (-+-) p q s = ((p -+- q) -*- s) -~- ((p -*- s) -+- (q -*- s))

sequentialCompositionIsAssociative = isAssociative (~) (<>)
parallelCompositionIsAssociative = isAssociative (~) (<||>)
parallelCompositionIsCommutative = isCommutative (~) (<||>)
sequentialCompositionDistributes = distributesOver (~) (<>) (<||>)

(~~) :: Policy -> Policy -> History -> Property
(~~) = testEquality applyPolicyTimely

timelySequentialCompositionIsAssociative = isAssociative (~~) (<>)
timelyParallelCompositionIsAssociative = isAssociative (~~) (<||>)
timelyParallelCompositionIsCommutative = isCommutative (~~) (<||>)
timelySequentialCompositionDistributes = distributesOver (~~) (<>) (<||>)

(~~~) :: Policy -> Policy -> History -> Property
(~~~) = testEquality applyPolicySteps

stepsSequentialCompositionIsAssociative = isAssociative (~~~) (<>)
stepsParallelCompositionIsAssociative = isAssociative (~~~) (<||>)
stepsParallelCompositionIsCommutative = isCommutative (~~~) (<||>)
stepsSequentialCompositionDistributes = distributesOver (~~~) (<>) (<||>)

qnkatQuickCheck :: Testable prop => prop -> IO ()
qnkatQuickCheck = quickCheckWith (stdArgs { maxSize = 4, maxSuccess = 10000 })
