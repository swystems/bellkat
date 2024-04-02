{-# OPTIONS_GHC -Wno-missing-signatures #-}

module BellKAT.Test where

import           Test.QuickCheck

import           GHC.Exts (toList)
import           Data.List         (intercalate)
import           Data.Set          (Set)
import           Data.Map.Strict (Map)
import           Data.Default

import           BellKAT.Definitions
import           BellKAT.Drawing

type Tag = Maybe Int

testEquality 
    :: (Show t, Eq t, Default t)
    => (Normal p t -> History t -> Set (History t))
    -> Normal p t -> Normal p t -> History t -> Property
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

(~*~) :: Normal OneRoundPolicy Tag -> Normal OneRoundPolicy Tag -> History Tag -> Property
(~*~) = testEquality applyOneStepPolicy

stepsSequentialCompositionIsAssociative = isAssociative (~~~) (<>)
stepsParallelCompositionIsAssociative = isAssociative (~~~) (<||>)
stepsParallelCompositionIsCommutative = isCommutative (~~~) (<||>)
stepsSequentialCompositionDistributes = distributesOver (~~~) (<>) (<||>)

bellkatQuickCheck :: Testable prop => prop -> IO ()
bellkatQuickCheck = quickCheckWith (stdArgs { maxSize = 4, maxSuccess = 10000 })

countQubitsAtLocation :: Ord tag => Location -> TaggedBellPairs tag -> Int
countQubitsAtLocation l = length . filter (hasLocation l . bellPair) . toList

-- | produces a predicate over valid states from upper bounds on the number of qubits at certain
-- locations. The state is valid if and only if /all/ the bounds are satisfied.
memoryBounds 
    :: Ord tag 
    => Map Location Int -- ^ Upper bounds on the number of qubits at respective locations
    -> TaggedBellPairs tag -> Bool
memoryBounds bounds bps = all (\(l,k) -> countQubitsAtLocation l bps <= k) $ toList bounds
