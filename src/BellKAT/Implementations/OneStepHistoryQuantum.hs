{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE UndecidableInstances #-}
module BellKAT.Implementations.OneStepHistoryQuantum
    ( OneStepPolicy(..)
    , FunctionStep (..)
    , FreeStep
    , execute
    , executePartial
    , executeFree
    ) where

import           Data.Functor.Compose         (Compose (..))
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Functor.Classes

import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Structures
import           BellKAT.Utils.Choice
import           BellKAT.Implementations.OneStepHistoryQuantum.FunctionStep
import           BellKAT.Implementations.OneStepHistoryQuantum.FreeStep

data OneStepPolicy a
    = Atomic a
    | Sequence (OneStepPolicy a) (OneStepPolicy a)
    | Choice (OneStepPolicy a) (OneStepPolicy a)
    deriving stock (Show)

instance Functor OneStepPolicy where
    fmap f (Atomic x) = Atomic (f x)
    fmap f (Sequence x y) = Sequence (fmap f x) (fmap f y)
    fmap f (Choice x y) = Choice (fmap f x) (fmap f y)

instance Show1 OneStepPolicy where
  liftShowsPrec sp _ d (Atomic x) = sp d x
  liftShowsPrec sp sl d (Sequence x y) =
      showParen (seq_prec < d) $
        liftShowsPrec sp sl (seq_prec + 1) x . showString " <.> "
        . liftShowsPrec sp sl (seq_prec + 1) y
    where
        seq_prec = 7
  liftShowsPrec sp sl d (Choice x y) =
      showParen (parallel_prec < d) $
        liftShowsPrec sp sl (parallel_prec + 1) x . showString " <+> "
        . liftShowsPrec sp sl (parallel_prec + 1) y
    where
        parallel_prec = 4

instance Semigroup (OneStepPolicy a) where
    (<>) = Sequence

instance (Monoid a) => Monoid (OneStepPolicy a) where
    mempty = Atomic mempty

chooseConcat :: NonEmpty (OneStepPolicy a) -> OneStepPolicy a
chooseConcat (x :| [])       = x
chooseConcat (x :| x' : xs') = Choice x (chooseConcat (x' :| xs'))

instance ParallelSemigroup (OneStepPolicy a) where
    p <||> q = chooseConcat $
        fmap (intermixAfterDecompsition q) (decompose p)
        <> fmap (intermixAfterDecompsition p) (decompose q)

instance OrderedSemigroup (OneStepPolicy a) where
    p <.> q = chooseConcat $
        fmap (intermixAfterDecompsition q) (decompose p)
        <> fmap (intermixAfterDecompsition p) (decompose q)

type Decomposition a = Either a (a, OneStepPolicy a)

decompose :: OneStepPolicy a -> NonEmpty (Either a (a, OneStepPolicy a))
decompose (Atomic x)     = [Left x]
decompose (Choice x y)   = decompose x <> decompose y
decompose (Sequence p q) = fmap (sequenceAfterDecomposition q) (decompose p)

intermixAfterDecompsition :: OneStepPolicy a -> Decomposition a  -> OneStepPolicy a
intermixAfterDecompsition q (Left x)        = Atomic x <> q
intermixAfterDecompsition q (Right (x, xs)) = Atomic x <> (q <||> xs)

sequenceAfterDecomposition :: OneStepPolicy a -> Decomposition a  -> Decomposition a
sequenceAfterDecomposition q (Left x)        = Right (x, q)
sequenceAfterDecomposition q (Right (x, xs)) = Right (x, Sequence xs q)


instance CreatesBellPairs a t =>  CreatesBellPairs (OneStepPolicy a) t where
    tryCreateBellPairFrom = Atomic . tryCreateBellPairFrom

instance (Ord t) => Quantum (OneStepPolicy (FunctionStep t)) t

instance (Ord tag, Show tag, Tests a test tag) => Tests (OneStepPolicy a) test tag where
  test = Atomic . test

instance (Ord tag, Show tag) => TestsQuantum (OneStepPolicy (FunctionStep tag)) BellPairsPredicate tag where

instance {-# OVERLAPPING #-} (Show1 f, Show a) => Show (Compose OneStepPolicy f a) where
    showsPrec d  (Compose x) = liftShowsPrec (liftShowsPrec showsPrec showList) (liftShowList showsPrec showList) d x

instance (Ord t) => ParallelSemigroup (Compose OneStepPolicy a t) where
  p <||> q = Compose $ getCompose p <||> getCompose q

instance (Ord t) => ChoiceSemigroup (Compose OneStepPolicy a t) where
  p <+> q = Compose $ getCompose p <||> getCompose q

instance (Ord t) => OrderedSemigroup (Compose OneStepPolicy a t) where
  p <.> q = Compose $ getCompose p <> getCompose q

instance (Ord t, CreatesBellPairs (a t) t) => CreatesBellPairs (Compose OneStepPolicy a t) t where
  tryCreateBellPairFrom = Compose . tryCreateBellPairFrom

instance (Ord t, CreatesBellPairs (a t) t) => Quantum (Compose OneStepPolicy a t) t where

instance (Show t, Ord t, Tests (a t) test t) => Tests (Compose OneStepPolicy a t) test t where
  test = Compose . test

instance (Show t, Ord t, Tests (a t) test t, CreatesBellPairs (a t) t)
  => TestsQuantum (Compose OneStepPolicy a t) test t where

executeOneStepPolicy :: (Ord tag) => OneStepPolicy (FunctionStep tag) -> FunctionStep tag
executeOneStepPolicy (Atomic x) = x
executeOneStepPolicy (Sequence p q)  = executeOneStepPolicy p <> executeOneStepPolicy q
executeOneStepPolicy (Choice p q)  = executeOneStepPolicy p <+> executeOneStepPolicy q

executePartial :: Ord tag => Compose OneStepPolicy FunctionStep tag -> History tag -> Set (Partial (History tag))
executePartial (Compose osp) = applyPartialNDEndo (executeFunctionStep (executeOneStepPolicy osp))

executeFree :: (Test test, Ord tag) => Compose OneStepPolicy (FreeStep test) tag -> History tag -> Set (History tag)
executeFree = execute . Compose . fmap runFreeStep . getCompose

execute :: Ord t => Compose OneStepPolicy FunctionStep t -> History t -> Set (History t)
execute p = Set.map unchoose . executePartial p
