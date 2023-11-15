{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module SpecSmall where

import Test.SmallCheck
import Test.SmallCheck.Series
import           Data.Functor.Contravariant (Predicate (..))
import           BellKAT.Definitions
import           BellKAT.Utils.UnorderedTree
import           Data.Multiset   (Multiset)
import qualified Data.Multiset   as Mset
import qualified Data.Set   as Set
import           Data.String (fromString)

instance Serial m a => Serial m (OneRoundPolicy a) where
    series = cons1 ORPAtomic \/ cons2 ORPSequence \/ cons2 ORPParallel \/ cons2 ORPChoice

instance (Ord a, Serial m a) => Serial m (Multiset a) where
    series = Mset.fromList <$> series

instance (Ord a, Serial m a) => Serial m (UTree a) where
    series = cons2 Node

instance Monad m => Serial m Location where
    series = fromString . (:[]) <$> series

instance Monad m => Serial m BellPair where
    series = (:~:) <$> localDepth (const 1) series <*> localDepth (const 1) series

instance Monad m => Serial m (TaggedBellPair ()) where
    series = TaggedBellPair <$> series <*> pure ()

instance Monad m => Serial m Action where
    series = cons2 Swap \/ cons2 Transmit \/ cons1 Distill \/ cons1 Create

instance Serial m t => Serial m (TaggedAction t) where
    series = TaggedAction (Predicate $ const True) <$> series <*> series <*> pure mempty

instance (Monad m) => Serial m (History ())  where
    series = History . Mset.fromList . map (`Node` Mset.empty) . concat <$> (replicate <$> series <*> series)

(~*~) :: Normal OneRoundPolicy () -> Normal OneRoundPolicy () -> History () -> Bool
p ~*~ q = \h -> applyOneStepPolicyPartial p h == applyOneStepPolicyPartial q h

(~<~) :: Normal OneRoundPolicy () -> Normal OneRoundPolicy () -> History () -> Bool
p ~<~ q = \h -> applyOneStepPolicyPartial p h `Set.isSubsetOf` applyOneStepPolicyPartial q h

main :: IO ()
main = 
     -- smallCheck 4 $ (p ~*~ q) ==> (p <||> r) ~*~ (q <||> r)
     -- smallCheck 4 $ \p q -> (p ~*~ q) ==> (\r -> (p <||> r) ~*~ (q <||> r))
     smallCheck 3 $ \a b (c :: TaggedAction ()) a' b' q -> 
         ((ORPAtomic a <> ORPAtomic b <> ORPAtomic c) ~<~ q)
           ==> 
        ((ORPAtomic a <> ORPAtomic a' <> ORPAtomic b' <> ORPAtomic b <> ORPAtomic c) ~<~ (q <||> (ORPAtomic a' <> ORPAtomic b')))

