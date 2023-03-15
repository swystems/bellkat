{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLists        #-}

module QNKAT.DSL where

import           Data.Functor.Contravariant

import           QNKAT.Definitions.Core     hiding (test, (<.>))
import           QNKAT.Definitions.Policy
import           QNKAT.UnorderedTree        (UTree (..))

distill :: DSLFunctions p => (Location, Location) -> p
distill locs = defaultTagged $ Distill locs

trans :: DSLFunctions p => Location -> (Location, Location) -> p
trans loc locs = defaultTagged $ Transmit loc locs

swap :: DSLFunctions p => Location -> (Location, Location) -> p
swap loc locs = defaultTagged $ Swap loc locs

create :: DSLFunctions p => Location -> p
create loc = defaultTagged $ Create loc

test :: Test (Maybe t) -> Ordered Policy (Maybe t)
test t = APAtomic [ ATest t ]

(~~?) :: Location -> Location -> Test (Maybe t)
l ~~? l' = any $ (== (l :~: l')) . bellPair

(/~?) :: Location -> Location -> Test (Maybe t)
l /~? l' = not . (l ~~? l')

class DSLFunctions p where
    defaultTagged :: Action -> p

instance DSLFunctions (Normal Policy (Maybe t)) where
    defaultTagged a = APAtomic $ TaggedAction mempty a Nothing mempty

instance DSLFunctions (Ordered Policy (Maybe t)) where
    defaultTagged a = APAtomic [ AAction (TaggedAction mempty a Nothing mempty) ]

(<.>) :: Ordered Policy a -> Ordered Policy a -> Ordered Policy a
(APAtomic tas) <.> (APAtomic tas') = APAtomic (tas <> tas')
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

instance Taggable (UTree (TaggedBellPair (Maybe t))) t where
    Node (TaggedBellPair bp _) ts .~ t = Node (TaggedBellPair bp (Just t)) ts

orP :: Predicate t -> Predicate t -> Predicate t
orP (Predicate f) (Predicate g) = Predicate ((||) <$> f <*> g)

(?~) :: PredicateLike p t => p -> Normal Policy (Maybe t) -> Normal Policy (Maybe t)
p ?~ APAtomic (TaggedAction _ a t dupKind) = APAtomic $
    TaggedAction (Predicate $ maybe True $ toPredicate p) a t dupKind
_ ?~ p                           = p

(.%) :: Normal Policy a -> DupKind -> Normal Policy a
APAtomic (TaggedAction p a t _) .% dk = APAtomic $ TaggedAction p a t dk
p .% _                                = p


node :: Ord t => BellPair -> UTree (TaggedBellPair (Maybe t))
node bp = Node (TaggedBellPair bp Nothing) []
