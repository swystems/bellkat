{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData   #-}
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
data AbstractPolicy a
    = APAtomic a
    | APSequence (AbstractPolicy a) (AbstractPolicy a)
    | APParallel (AbstractPolicy a) (AbstractPolicy a)
    deriving stock (Show)

type Policy t = AbstractPolicy (TaggedAction t)

instance Semigroup (AbstractPolicy a) where
    (<>) = APSequence

instance ParallelSemigroup (AbstractPolicy a) where
    (<||>) = APParallel

type OrderedPolicy t = AbstractPolicy (NonEmpty (TaggedAction t))

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

instance (Arbitrary a) => Arbitrary (AbstractPolicy a) where
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
