{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedLists        #-}
{-# LANGUAGE FunctionalDependencies #-}

module QNKAT.DSL where

import           Data.Functor.Contravariant

import           QNKAT.Definitions
import           QNKAT.UnorderedTree        (UTree (..))

defaultTagged :: Action -> Policy (Maybe t)
defaultTagged a = Atomic $ TaggedAction mempty a Nothing (DupKind False)

distill :: (Location, Location) -> Policy (Maybe t)
distill locs = defaultTagged $ Distill locs

trans :: Location -> (Location, Location) -> Policy (Maybe t)
trans loc locs = defaultTagged $ Transmit loc locs

swap :: Location -> (Location, Location) -> Policy (Maybe t)
swap loc locs = defaultTagged $ Swap loc locs

create :: Location -> Policy (Maybe t)
create loc = defaultTagged $ Create loc

dup :: Policy a -> Policy a
dup (Atomic (TaggedAction pt a t _)) = Atomic $
    TaggedAction pt a t (DupKind True)
dup p = p

class PredicateLike p t where
    toPredicate :: p -> t -> Bool

instance PredicateLike (t -> Bool) t where
    toPredicate = id

instance (Eq t, Foldable f) => PredicateLike (f t) t where
    toPredicate ts = (`elem` ts)

class Taggable a t | a -> t where
    (.~) :: a -> t -> a

instance Taggable (Policy (Maybe t)) t where
    Atomic (TaggedAction p a _ dupKind) .~ t = Atomic (TaggedAction p a (Just t) dupKind)
    p .~ _                           = p

instance Taggable (UTree (TaggedBellPair (Maybe t))) t where
    Node (bp, _) ts .~ t = Node (bp, Just t) ts

orP :: Predicate t -> Predicate t -> Predicate t
orP (Predicate f) (Predicate g) = Predicate ((||) <$> f <*> g) 

(?~) :: PredicateLike p t => p -> Policy (Maybe t) -> Policy (Maybe t)
p ?~ Atomic (TaggedAction _ a t dupKind) = Atomic $ 
    TaggedAction (Predicate $ maybe True $ toPredicate p) a t dupKind
_ ?~ p                           = p

node :: Ord t => BellPair -> UTree (TaggedBellPair (Maybe t))
node bp = Node (bp, Nothing) []
