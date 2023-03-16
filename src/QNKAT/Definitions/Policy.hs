{-# LANGUAGE StrictData #-}
module QNKAT.Definitions.Policy where

import           Data.Functor.Contravariant (Predicate (..))

import           Test.QuickCheck            hiding (choose)

import           Data.List.NonEmpty         (NonEmpty)
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
data Policy a
    = APAtomic a
    | APSequence (Policy a) (Policy a)
    | APParallel (Policy a) (Policy a)
    deriving stock (Show)

data FullPolicy a
    = FPAtomic a
    | FPSequence (FullPolicy a) (FullPolicy a)
    | FPParallel (FullPolicy a) (FullPolicy a)
    | FPOne
    | FPChoice (FullPolicy a) (FullPolicy a)
    deriving stock (Show)

instance Semigroup (Policy a) where
    (<>) = APSequence

instance Semigroup (FullPolicy a) where
    (<>) = FPSequence

instance ParallelSemigroup (FullPolicy a) where
    (<||>) = FPParallel

instance Monoid (FullPolicy a) where
    mempty = FPOne

instance ParallelSemigroup (Policy a) where
    (<||>) = APParallel

instance ChoiceSemigroup (FullPolicy a) where
    (<+>) = FPChoice

data Atomic t = AAction (TaggedAction t) | ATest (Test t)

type Normal p t = p (TaggedAction t)
type Ordered p t = p (NonEmpty (Atomic t))

-- * Testing definitions

instance Arbitrary Action where
    arbitrary = oneof
        [ Swap <$> arbitrary <*> arbitrary
        , Create <$> arbitrary
        , Transmit <$> arbitrary <*> arbitrary
        , Distill <$> arbitrary
        ]

instance (Arbitrary t, Eq t) => Arbitrary (TaggedAction t) where
  arbitrary = do
    predicate :: [t] <- arbitrary
    TaggedAction (Predicate (`elem` predicate)) <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (Policy a) where
    arbitrary = do
        n <- getSize
        if n == 0 then
            resize 1 $ APAtomic <$> arbitrary
        else
            resize (n - 1) $ oneof [
                APSequence <$> arbitrary <*> arbitrary,
                APParallel <$> arbitrary <*> arbitrary
            ]

    shrink (APSequence p q) = [p, q]
    shrink (APParallel p q) = [p, q]
    shrink _                = []
