{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLists        #-}

module BellKAT.DSL where

import           Data.Functor.Contravariant
import           Data.Default
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

ucreate :: DSLFunctions p => (Location, Location) -> p
ucreate loc = defaultTagged $ UnstableCreate loc

(~) :: Default tag => Location -> Location -> TaggedBellPair tag
l1 ~ l2 = TaggedBellPair (l1 :~: l2) def

class DSLTestNeq t where
    (/~?) :: Location -> Location -> t

class DSLTestNeq t => DSLTest t where
    (~~?) :: Location -> Location -> t

instance DSLTestNeq (BellPairsPredicate (Maybe t)) where
    l /~? l' = BPsPredicate $ not . any ((== (l :~: l')) . bellPair)

instance DSLTest (BellPairsPredicate (Maybe t)) where
    l ~~? l' = BPsPredicate $ any $ (== (l :~: l')) . bellPair

instance Ord t => DSLTestNeq (FreeTest (Maybe t)) where
    l /~? l' = FTNot $ FTSubset [TaggedBellPair (l :~: l') Nothing]

instance Ord t => DSLTest (FreeTest (Maybe t)) where
    l ~~? l' = FTSubset [TaggedBellPair (l :~: l') Nothing]

class DSLFunctions p where
    defaultTagged :: Action -> p
    (.%) :: p -> DupKind -> p

class DSLTestFunctions p test t | p -> t, p -> test where
    test :: test (Maybe t) -> p

-- test :: Test (Maybe t) -> Ordered Policy (Maybe t)
instance DSLTestFunctions (Ordered Policy test (Maybe tag)) test tag where
    test t = APAtomic [ ATest t ]

instance DSLTestFunctions (Ordered FullPolicy test (Maybe tag)) test tag where
    test t = FPAtomic [ ATest t ]

instance DSLTestFunctions (Ordered StarPolicy test (Maybe tag)) test tag where
    test t = SPAtomic [ ATest t ]

instance DSLTestFunctions (NormalWithTests StarPolicy test (Maybe tag)) test tag where
    test t = SPAtomic (ATest t)

instance DSLFunctions (Normal Policy (Maybe tag)) where
    defaultTagged a = APAtomic $ TaggedAction mempty a Nothing mempty

    APAtomic (TaggedAction p a t _) .% dk = APAtomic $ TaggedAction p a t dk
    _ .% _                                = error "cannot attach dup to this thing"

instance DSLFunctions (Ordered Policy test (Maybe tag)) where
    defaultTagged a = APAtomic [ AAction (TaggedAction mempty a Nothing mempty) ]

    APAtomic (AAction (TaggedAction p a t _) :| []) .% dk = APAtomic [ AAction (TaggedAction p a t dk) ]
    _ .% _                                = error "cannot attach dup to this thing"

instance DSLFunctions (Normal StarPolicy (Maybe tag)) where
    defaultTagged a = SPAtomic $ TaggedAction mempty a Nothing mempty
    _ .% _                                = error "cannot attach dup to this thing"

instance DSLFunctions (NormalWithTests StarPolicy test (Maybe tag)) where
    defaultTagged a = SPAtomic $ AAction $ TaggedAction mempty a Nothing mempty
    _ .% _                                = error "cannot attach dup to this thing"

instance DSLFunctions (Ordered FullPolicy test (Maybe tag)) where
    defaultTagged a = FPAtomic [ AAction (TaggedAction mempty a Nothing mempty) ]

    FPAtomic (AAction (TaggedAction p a t _) :| []) .% dk = FPAtomic [ AAction (TaggedAction p a t dk) ]
    _ .% _                                = error "cannot attach dup to this thing"

instance DSLFunctions (Ordered StarPolicy test (Maybe tag)) where
    defaultTagged a = SPAtomic [ AAction (TaggedAction mempty a Nothing mempty) ]

    SPAtomic (AAction (TaggedAction p a t _) :| []) .% dk = SPAtomic [ AAction (TaggedAction p a t dk) ]
    _ .% _                                = error "cannot attach dup to this thing"

instance DSLFunctions (Ordered StarPolicy test ()) where
    defaultTagged a = SPAtomic [ AAction (TaggedAction mempty a () mempty) ]

    SPAtomic (AAction (TaggedAction p a t _) :| []) .% dk = SPAtomic [ AAction (TaggedAction p a t dk) ]
    _ .% _                                = error "cannot attach dup to this thing"

infixl 7 <.>

class DSLOrderedSemigroup a where
    (<.>) :: a -> a -> a

instance DSLOrderedSemigroup (Ordered Policy test tag) where
    (APAtomic tas) <.> (APAtomic tas') = APAtomic (tas <> tas')
    _ <.> _                            = error "Can only compose atomics with <.>"

instance DSLOrderedSemigroup (Ordered FullPolicy test tag) where
    (FPAtomic tas) <.> (FPAtomic tas') = FPAtomic (tas <> tas')
    _ <.> _                            = error "Can only compose atomics with <.>"

instance DSLOrderedSemigroup (Ordered StarPolicy test tag) where
    (SPAtomic tas) <.> (SPAtomic tas') = SPAtomic (tas <> tas')
    _ <.> _                            = error "Can only compose atomics with <.>"

instance DSLOrderedSemigroup (NormalWithTests StarPolicy test tag) where
    (<.>) = SPOrdered

instance DSLOrderedSemigroup (Normal StarPolicy tag) where
    p <.> p' = SPOrdered p p'

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

instance (Eq tag, Taggable (test (Maybe tag)) tag) 
        => Taggable (Ordered StarPolicy test (Maybe tag)) tag where
    SPAtomic (AAction (TaggedAction p a _ dupKind) :| []) .~ t = 
        SPAtomic $ AAction (TaggedAction p a (Just t) dupKind) :| []
    SPAtomic (ATest t :| []) .~ tag = 
        SPAtomic $ ATest (t .~ tag) :| []
    p .~ _                           = p

instance Eq t => Taggable (BellPairsPredicate (Maybe t)) t where
    t .~ tag = BPsPredicate $ \bps -> all (hasTag tag) bps && getBPsPredicate t bps

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
