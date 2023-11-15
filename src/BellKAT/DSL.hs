{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLists        #-}

module BellKAT.DSL where

import           Data.Functor.Contravariant
import           Data.List.NonEmpty         (NonEmpty (..))

import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Policy
import           BellKAT.Utils.UnorderedTree         (UTree (..))

distill :: DSLFunctions p => (Location, Location) -> p
distill locs = defaultTagged $ Distill locs

trans :: DSLFunctions p => Location -> (Location, Location) -> p
trans loc locs = defaultTagged $ Transmit loc locs

swap :: DSLFunctions p => Location -> (Location, Location) -> p
swap loc locs = defaultTagged $ Swap loc locs

create :: DSLFunctions p => Location -> p
create loc = defaultTagged $ Create loc

(~~?) :: Location -> Location -> Test (Maybe t)
l ~~? l' = any $ (== (l :~: l')) . bellPair

(/~?) :: Location -> Location -> Test (Maybe t)
l /~? l' = not . (l ~~? l')

class DSLFunctions p where
    defaultTagged :: Action -> p
    (.%) :: p -> DupKind -> p

class DSLTestFunctions p t | p -> t where
    test :: Test (Maybe t) -> p

-- test :: Test (Maybe t) -> Ordered Policy (Maybe t)
instance DSLTestFunctions (Ordered Policy (Maybe t)) t where
    test t = APAtomic [ ATest t ]

instance DSLTestFunctions (Ordered FullPolicy (Maybe t)) t where
    test t = FPAtomic [ ATest t ]

instance DSLTestFunctions (Ordered StarPolicy (Maybe t)) t where
    test t = SPAtomic [ ATest t ]

instance DSLFunctions (Normal Policy (Maybe t)) where
    defaultTagged a = APAtomic $ TaggedAction mempty a Nothing mempty

    APAtomic (TaggedAction p a t _) .% dk = APAtomic $ TaggedAction p a t dk
    _ .% _                                = error "cannot attach dup to this thing"

instance DSLFunctions (Ordered Policy (Maybe t)) where
    defaultTagged a = APAtomic [ AAction (TaggedAction mempty a Nothing mempty) ]

    APAtomic (AAction (TaggedAction p a t _) :| []) .% dk = APAtomic [ AAction (TaggedAction p a t dk) ]
    _ .% _                                = error "cannot attach dup to this thing"

instance DSLFunctions (Ordered FullPolicy (Maybe t)) where
    defaultTagged a = FPAtomic [ AAction (TaggedAction mempty a Nothing mempty) ]

    FPAtomic (AAction (TaggedAction p a t _) :| []) .% dk = FPAtomic [ AAction (TaggedAction p a t dk) ]
    _ .% _                                = error "cannot attach dup to this thing"

instance DSLFunctions (Ordered StarPolicy (Maybe t)) where
    defaultTagged a = SPAtomic [ AAction (TaggedAction mempty a Nothing mempty) ]

    SPAtomic (AAction (TaggedAction p a t _) :| []) .% dk = SPAtomic [ AAction (TaggedAction p a t dk) ]
    _ .% _                                = error "cannot attach dup to this thing"

instance DSLFunctions (Ordered StarPolicy ()) where
    defaultTagged a = SPAtomic [ AAction (TaggedAction mempty a () mempty) ]

    SPAtomic (AAction (TaggedAction p a t _) :| []) .% dk = SPAtomic [ AAction (TaggedAction p a t dk) ]
    _ .% _                                = error "cannot attach dup to this thing"

infixl 7 <.>

class DSLOrderedSemigroup a where
    (<.>) :: a -> a -> a

instance DSLOrderedSemigroup (Ordered Policy a) where
    (APAtomic tas) <.> (APAtomic tas') = APAtomic (tas <> tas')
    _ <.> _                            = error "Can only compose atomics with <.>"

instance DSLOrderedSemigroup (Ordered FullPolicy a) where
    (FPAtomic tas) <.> (FPAtomic tas') = FPAtomic (tas <> tas')
    _ <.> _                            = error "Can only compose atomics with <.>"

instance DSLOrderedSemigroup (Ordered StarPolicy a) where
    (SPAtomic tas) <.> (SPAtomic tas') = SPAtomic (tas <> tas')
    _ <.> _                            = error "Can only compose atomics with <.>"

dupA :: DupKind
dupA = DupKind { dupBefore = False, dupAfter = True }

dupB :: DupKind
dupB = DupKind { dupBefore = True, dupAfter = False }

class PredicateLike p t where
    toPredicate :: p -> t -> Bool

instance PredicateLike (t -> Bool) t where
    toPredicate = id

instance (Eq t, Foldable f) => PredicateLike (f t) t where
    toPredicate ts = (`elem` ts)

class Taggable a t | a -> t where
    (.~) :: a -> t -> a

instance Taggable (Normal Policy (Maybe t)) t where
    APAtomic (TaggedAction p a _ dupKind) .~ t = APAtomic (TaggedAction p a (Just t) dupKind)
    p .~ _                           = p

instance Eq t => Taggable (Ordered StarPolicy (Maybe t)) t where
    SPAtomic (AAction (TaggedAction p a _ dupKind) :| []) .~ t = 
        SPAtomic $ AAction (TaggedAction p a (Just t) dupKind) :| []
    SPAtomic (ATest t :| []) .~ tag = 
        SPAtomic $ ATest (t .~ tag) :| []
    p .~ _                           = p

instance Eq t => Taggable (Test (Maybe t)) t where
    t .~ tag = \bps -> all (hasTag tag) bps && t bps

hasTag :: Eq t => t -> TaggedBellPair (Maybe t) -> Bool
hasTag tag tbp = Just tag == bellPairTag tbp

instance Taggable (UTree (TaggedBellPair (Maybe t))) t where
    Node (TaggedBellPair bp _) ts .~ t = Node (TaggedBellPair bp (Just t)) ts

orP :: Predicate t -> Predicate t -> Predicate t
orP (Predicate f) (Predicate g) = Predicate ((||) <$> f <*> g)

(?~) :: PredicateLike p t => p -> Normal Policy (Maybe t) -> Normal Policy (Maybe t)
p ?~ APAtomic (TaggedAction _ a t dupKind) = APAtomic $
    TaggedAction (Predicate $ maybe True $ toPredicate p) a t dupKind
_ ?~ p                           = p


node :: Ord t => BellPair -> UTree (TaggedBellPair (Maybe t))
node bp = Node (TaggedBellPair bp Nothing) []
