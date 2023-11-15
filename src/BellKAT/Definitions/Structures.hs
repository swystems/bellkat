module BellKAT.Definitions.Structures
    ( ParallelSemigroup(..)
    , ChoiceSemigroup(..)
    , Quantum
    , CreatesBellPairs(..)
    , Tests(..)
    , TestsQuantum
    , TestsOrderedQuantum(..)
    , OrderedQuantum(..)
    , OrderedSemigroup(..)
    , MonoidStar(..)
    , subjectTo
    ) where

import           Data.Functor.Contravariant (Predicate (..))
import           Data.Orphans ()

import           BellKAT.Definitions.Core
        --
-- parallel composition is left-associative and has lower precedence than `<>`
infixl 5 <||>

class (ChoiceSemigroup a, Monoid a) => MonoidStar a where
    star :: a -> a

-- | Define alg structure `ParallelSemigroup` with `<>` inherited from
-- `Semigroup` and new `<||>` for parallel composition
class ParallelSemigroup a where
    (<||>) :: a -> a -> a

-- choice is left-associative and has lower precedence than `<||>` or `<>`
infixl 4 <+>

class ChoiceSemigroup a where
    (<+>) :: a -> a -> a

class OrderedSemigroup a where
    (<.>) :: a -> a -> a

class CreatesBellPairs a t | a -> t where
    -- | is function from `BellPair`, `[BellPair]` to `Maybe Double` Quantum;
    -- will be used in `meaning` of `Distill`
    tryCreateBellPairFrom :: CreateBellPairArgs t -> a

-- | `Quantum` is a `ParallelSemigroup` with `BellPair` creation
-- `a` is the type of the carrier and `t` is a tag
class (Semigroup a, ParallelSemigroup a, CreatesBellPairs a t) => Quantum a t | a -> t where

class Tests a t | a -> t where
    test :: Test t -> a

class (Quantum a t, Tests a t) => TestsQuantum a t | a -> t where

-- | `Quantum` that has two domains: 
--  * `a` for the top-level behavior having `Semigroup` and `ParallelSemigroup` structures)
--  * `Layer a` for the one-layer behavior having `OrderedSemigroup` structure
-- `liftLayer` is an embedding from a Layer
class (Semigroup a, ParallelSemigroup a, OrderedSemigroup (Layer a)) => OrderedQuantum a t | a -> t where
    data Layer a

    orderedTryCreateBellPairFrom :: CreateBellPairArgs t -> Layer a
    liftLayer :: Layer a -> a

class OrderedQuantum a t => TestsOrderedQuantum a t where
    orderedTest :: Test t -> Layer a

-- | Notation for predicate
subjectTo :: Quantum a t => Predicate t -> (Predicate t -> a) -> a
subjectTo pt f = f pt
