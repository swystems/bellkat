{-# OPTIONS_GHC -Wno-missing-signatures #-}

module QNKAT.Test where

import           Test.QuickCheck

import           Data.List         (intercalate)
import           Data.Set          (Set)

import           QNKAT.Definitions
import           QNKAT.Drawing

type Tag = Maybe Int

testEquality 
    :: (Show t, Eq t)
    => (Normal Policy t -> History t -> Set (History t))
    -> Normal Policy t -> Normal Policy t -> History t -> Property
testEquality apply p q h =
    let hsP = apply p h
        hsQ = apply q h

        counterexampleText = intercalate "\n" $
               [drawHistoriesText hsP]
            <> ["   =/=   "]
            <> [drawHistoriesText hsQ]
     in counterexample counterexampleText (hsP == hsQ)


(~) :: Normal Policy Tag -> Normal Policy Tag -> History Tag -> Property
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

(~~) :: Normal Policy Tag -> Normal Policy Tag -> History Tag -> Property
(~~) = testEquality applyPolicyTimely

timelySequentialCompositionIsAssociative = isAssociative (~~) (<>)
timelyParallelCompositionIsAssociative = isAssociative (~~) (<||>)
timelyParallelCompositionIsCommutative = isCommutative (~~) (<||>)
timelySequentialCompositionDistributes = distributesOver (~~) (<>) (<||>)

(~~~) :: Normal Policy Tag -> Normal Policy Tag -> History Tag -> Property
(~~~) = testEquality applyPolicySteps

stepsSequentialCompositionIsAssociative = isAssociative (~~~) (<>)
stepsParallelCompositionIsAssociative = isAssociative (~~~) (<||>)
stepsParallelCompositionIsCommutative = isCommutative (~~~) (<||>)
stepsSequentialCompositionDistributes = distributesOver (~~~) (<>) (<||>)

qnkatQuickCheck :: Testable prop => prop -> IO ()
qnkatQuickCheck = quickCheckWith (stdArgs { maxSize = 4, maxSuccess = 10000 })
