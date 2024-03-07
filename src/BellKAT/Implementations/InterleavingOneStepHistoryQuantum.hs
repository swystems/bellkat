{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE UndecidableInstances #-}
module BellKAT.Implementations.InterleavingOneStepHistoryQuantum
    ( InterleavingOneStepPolicy(..)
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
import           BellKAT.Implementations.InterleavingOneStepHistoryQuantum.FunctionStep
import           BellKAT.Implementations.InterleavingOneStepHistoryQuantum.FreeStep

data InterleavingOneStepPolicy a
    = Atomic a
    | Sequence (InterleavingOneStepPolicy a) (InterleavingOneStepPolicy a)
    | Choice (InterleavingOneStepPolicy a) (InterleavingOneStepPolicy a)
    deriving stock (Show)

instance Functor InterleavingOneStepPolicy where
    fmap f (Atomic x) = Atomic (f x)
    fmap f (Sequence x y) = Sequence (fmap f x) (fmap f y)
    fmap f (Choice x y) = Choice (fmap f x) (fmap f y)

instance Show1 InterleavingOneStepPolicy where
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

instance Semigroup (InterleavingOneStepPolicy a) where
    (<>) = Sequence

instance (Monoid a) => Monoid (InterleavingOneStepPolicy a) where
    mempty = Atomic mempty

chooseConcat :: NonEmpty (InterleavingOneStepPolicy a) -> InterleavingOneStepPolicy a
chooseConcat (x :| [])       = x
chooseConcat (x :| x' : xs') = Choice x (chooseConcat (x' :| xs'))

instance ParallelSemigroup (InterleavingOneStepPolicy a) where
    p <||> q = chooseConcat $
        fmap (intermixAfterDecompsition q) (decompose p)
        <> fmap (intermixAfterDecompsition p) (decompose q)

instance OrderedSemigroup (InterleavingOneStepPolicy a) where
    p <.> q = chooseConcat $
        fmap (intermixAfterDecompsition q) (decompose p)
        <> fmap (intermixAfterDecompsition p) (decompose q)

type Decomposition a = Either a (a, InterleavingOneStepPolicy a)

decompose :: InterleavingOneStepPolicy a -> NonEmpty (Either a (a, InterleavingOneStepPolicy a))
decompose (Atomic x)     = [Left x]
decompose (Choice x y)   = decompose x <> decompose y
decompose (Sequence p q) = fmap (sequenceAfterDecomposition q) (decompose p)

intermixAfterDecompsition :: InterleavingOneStepPolicy a -> Decomposition a  -> InterleavingOneStepPolicy a
intermixAfterDecompsition q (Left x)        = Atomic x <> q
intermixAfterDecompsition q (Right (x, xs)) = Atomic x <> (q <||> xs)

sequenceAfterDecomposition :: InterleavingOneStepPolicy a -> Decomposition a  -> Decomposition a
sequenceAfterDecomposition q (Left x)        = Right (x, q)
sequenceAfterDecomposition q (Right (x, xs)) = Right (x, Sequence xs q)


instance CreatesBellPairs a t =>  CreatesBellPairs (InterleavingOneStepPolicy a) t where
    tryCreateBellPairFrom = Atomic . tryCreateBellPairFrom

instance (Ord t) => Quantum (InterleavingOneStepPolicy (FunctionStep t)) t

instance (Ord tag, Show tag, Tests a test tag) => Tests (InterleavingOneStepPolicy a) test tag where
  test = Atomic . test

instance (Ord tag, Show tag) => TestsQuantum (InterleavingOneStepPolicy (FunctionStep tag)) BellPairsPredicate tag where

instance {-# OVERLAPPING #-} (Show1 f, Show a) => Show (Compose InterleavingOneStepPolicy f a) where
    showsPrec d  (Compose x) = liftShowsPrec (liftShowsPrec showsPrec showList) (liftShowList showsPrec showList) d x

instance (Ord t) => ParallelSemigroup (Compose InterleavingOneStepPolicy a t) where
  p <||> q = Compose $ getCompose p <||> getCompose q

instance (Ord t) => ChoiceSemigroup (Compose InterleavingOneStepPolicy a t) where
  p <+> q = Compose $ getCompose p <||> getCompose q

instance (Ord t) => OrderedSemigroup (Compose InterleavingOneStepPolicy a t) where
  p <.> q = Compose $ getCompose p <> getCompose q

instance (Ord t, CreatesBellPairs (a t) t) => CreatesBellPairs (Compose InterleavingOneStepPolicy a t) t where
  tryCreateBellPairFrom = Compose . tryCreateBellPairFrom

instance (Ord t, CreatesBellPairs (a t) t) => Quantum (Compose InterleavingOneStepPolicy a t) t where

instance (Show t, Ord t, Tests (a t) test t) => Tests (Compose InterleavingOneStepPolicy a t) test t where
  test = Compose . test

instance (Show t, Ord t, Tests (a t) test t, CreatesBellPairs (a t) t)
  => TestsQuantum (Compose InterleavingOneStepPolicy a t) test t where

executeOneStepPolicy :: (Ord tag) => InterleavingOneStepPolicy (FunctionStep tag) -> FunctionStep tag
executeOneStepPolicy (Atomic x) = x
executeOneStepPolicy (Sequence p q)  = executeOneStepPolicy p <> executeOneStepPolicy q
executeOneStepPolicy (Choice p q)  = executeOneStepPolicy p <+> executeOneStepPolicy q

executePartial :: Ord tag => Compose InterleavingOneStepPolicy FunctionStep tag -> History tag -> Set (Partial (History tag))
executePartial (Compose osp) = applyPartialNDEndo (executeFunctionStep (executeOneStepPolicy osp))

executeFree :: (Test test, Ord tag) => Compose InterleavingOneStepPolicy (FreeStep test) tag -> History tag -> Set (History tag)
executeFree = execute . Compose . fmap runFreeStep . getCompose

execute :: Ord t => Compose InterleavingOneStepPolicy FunctionStep t -> History t -> Set (History t)
execute p = Set.map unchoose . executePartial p
