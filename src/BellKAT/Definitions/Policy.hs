{-# LANGUAGE StrictData #-}
module BellKAT.Definitions.Policy where

import           Data.Functor.Contravariant (Predicate (..))
import           Test.QuickCheck            hiding (choose)
import           Data.List.NonEmpty         (NonEmpty)

import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Structures

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

-- | Define a language for policies, where `a` is the type of an atomic action
data Policy a
    = APAtomic a
    | APSequence (Policy a) (Policy a)
    | APParallel (Policy a) (Policy a)
    deriving stock (Show)

data OneRoundPolicy a
    = ORPAtomic a
    | ORPSequence (OneRoundPolicy a) (OneRoundPolicy a)
    | ORPParallel (OneRoundPolicy a) (OneRoundPolicy a)
    | ORPChoice (OneRoundPolicy a) (OneRoundPolicy a)

instance Show a => Show (OneRoundPolicy a) where
    showsPrec _ (ORPAtomic x) = shows x
    showsPrec d (ORPParallel x y) = showParen (d > 5) $ 
        showsPrec 6 x . showString " <||> " . showsPrec 6 y
    showsPrec d (ORPSequence x y) = showParen (d > 6) $ 
        showsPrec 7 x . showString " <> " . showsPrec 7 y
    showsPrec d (ORPChoice x y) = showParen (d > 4) $ 
        showsPrec 5 x . showString " <+> " . showsPrec 5 y

data FullPolicy a
    = FPAtomic a
    | FPSequence (FullPolicy a) (FullPolicy a)
    | FPParallel (FullPolicy a) (FullPolicy a)
    | FPOne
    | FPChoice (FullPolicy a) (FullPolicy a)
    deriving stock (Show)

data StarPolicy a
    = SPAtomic a
    | SPSequence (StarPolicy a) (StarPolicy a)
    | SPParallel (StarPolicy a) (StarPolicy a)
    | SPOrdered (StarPolicy a) (StarPolicy a)
    | SPOne
    | SPStar (StarPolicy a)
    | SPChoice (StarPolicy a) (StarPolicy a)
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

instance ChoiceSemigroup (OneRoundPolicy a) where
    (<+>) = ORPChoice

instance Semigroup (OneRoundPolicy a) where
    (<>) = ORPSequence

instance ParallelSemigroup (OneRoundPolicy a) where
    (<||>) = ORPParallel

instance ChoiceSemigroup (FullPolicy a) where
    (<+>) = FPChoice

instance Semigroup (StarPolicy a) where
    (<>) = SPSequence

instance Monoid (StarPolicy a) where
    mempty = SPOne

instance MonoidStar (StarPolicy a) where
    star = SPStar

instance ParallelSemigroup (StarPolicy a) where
    (<||>) = SPParallel

instance ChoiceSemigroup (StarPolicy a) where
    (<+>) = SPChoice

instance OrderedSemigroup (StarPolicy a) where
    (<.>) = SPOrdered

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

instance (Arbitrary a) => Arbitrary (OneRoundPolicy a) where
    arbitrary = do
        n <- getSize
        if n == 0 then
            resize 1 $ ORPAtomic <$> arbitrary
        else
            resize (n - 1) $ oneof [
                ORPSequence <$> arbitrary <*> arbitrary,
                ORPParallel <$> arbitrary <*> arbitrary
            ]
    shrink (ORPSequence p q) = [p, q]
    shrink (ORPParallel p q) = [p, q]
    shrink _                = []
