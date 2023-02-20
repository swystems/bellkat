{-# LANGUAGE StrictData #-}
module QNKAT.Definitions.Policy where

import           Data.Functor.Contravariant (Predicate (..))

import           Test.QuickCheck            hiding (choose)

import           QNKAT.Definitions.Core

-- * Main policy definitions

-- | Define primitive actions
data Action
    = Swap Location (Location, Location)
    | Transmit Location (Location, Location)
    | Distill (Location, Location)
    | Create Location
    deriving stock (Show)

data TaggedAction t = TaggedAction
    { taTagPredicate :: Predicate t
    , taAction       :: Action
    , taTag          :: t
    , taDup          :: DupKind
    }

instance Show t => Show (TaggedAction t) where
    show ta = "_:" <> show (taAction ta) <> ":" <> show (taTag ta)

-- | Define policy
data Policy t
    = Atomic (TaggedAction t)
    | Sequence (Policy t) (Policy t)
    | Parallel (Policy t) (Policy t)
    deriving stock (Show)

instance Semigroup (Policy t) where
    (<>) = Sequence

instance ParallelSemigroup (Policy t) where
    (<||>) = Parallel
--
-- * Testing definitions

instance Arbitrary Action where
    arbitrary = oneof
        [ Swap <$> arbitrary <*> arbitrary
        , Create <$> arbitrary
        , Transmit <$> arbitrary <*> arbitrary
        , Distill <$> arbitrary
        ]

instance (Arbitrary t, Eq t) => Arbitrary (Policy t) where
    arbitrary = do
        n <- getSize
        if n == 0 then
            resize 1 $ do
                predicate :: [t] <- arbitrary
                Atomic <$> (TaggedAction (Predicate (`elem` predicate)) <$> arbitrary <*> arbitrary <*> arbitrary)
        else
            resize (n - 1) $ oneof [
                Sequence <$> arbitrary <*> arbitrary,
                Sequence <$> arbitrary <*> arbitrary
            ]

    shrink (Sequence p q) = [p, q]
    shrink (Parallel p q) = [p, q]
    shrink _              = []
