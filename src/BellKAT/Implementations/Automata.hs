{-# LANGUAGE StrictData #-}
module BellKAT.Implementations.Automata (MagicNFA(..), productWith) where

import           Data.IntMap.Strict           (IntMap)
import qualified Data.IntMap.Strict           as IM
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IS
import           Data.List
import qualified Data.Map.Strict              as Map
import           Data.Pointed
import qualified Data.Set                     as Set
import           Data.These
import           Data.Foldable (toList)
import           Data.These.Combinators       (isThat, justThat)
import           Data.Graph

import           BellKAT.Definitions.Structures

data Eps = Eps deriving stock (Show)

data EpsNFA a = ENFA
    { enfaInitial    :: Int
    , enfaTransition :: !(IntMap (IntMap (These Eps a)))
    , enfaFinal      :: !IntSet
    }

data MagicNFA a = MNFA
    { mnfaInitial    :: Int
    , mnfaTransition :: !(IntMap (IntMap a))
    , mnfaFinal      :: !IntSet
    }

showTransition :: Show a => (Int, a) -> String
showTransition (j , act) = "-( " <> show act <> " )-> " <> show j

showEpsTransition :: Show a => (Int, These Eps a) -> String
showEpsTransition (j, This Eps) = "-()-> " <> show j
showEpsTransition (j, These Eps act) = "-( eps | " <> show act <> " )-> " <> show j
showEpsTransition (j, That x) = showTransition (j, x)

instance Show a => Show (MagicNFA a) where
    show x = unlines $
        map showState $ IM.toList (mnfaTransition x)
      where
        showState (s, sTr) = 
            (if s == mnfaInitial x then "^" else "") 
            <> show s 
            <> (if IS.member s (mnfaFinal x) then "$" else "")
            <> ": "
            <> unwords (map showTransition $ IM.toList sTr)

instance Show a => Show (EpsNFA a) where
    show x = unlines $
        map showState $ IM.toList (enfaTransition x)
      where
        showState (s, sTr) = 
            (if s == enfaInitial x then "^" else "") 
            <> show s 
            <> (if IS.member s (enfaFinal x) then "$" else "")
            <> ": "
            <> unwords (map showEpsTransition $ IM.toList sTr)

shiftFinalUp :: Int -> IntSet -> IntSet
shiftFinalUp k = IS.mapMonotonic (+ k)

shiftTransitionUp :: Int -> IntMap (IntMap a) -> IntMap (IntMap a)
shiftTransitionUp k = IM.map (IM.mapKeysMonotonic (+ k)) . IM.mapKeysMonotonic (+k)

unionTransition
    :: ChoiceSemigroup a
    => IntMap (IntMap a) -> IntMap (IntMap a) -> IntMap (IntMap a)
unionTransition = IM.unionWith (IM.unionWith (<+>))

instance (ChoiceSemigroup a) => ChoiceSemigroup (These Eps a) where
    This Eps <+> This Eps       = This Eps
    This Eps <+> That x         = These Eps x
    This Eps <+> These Eps x    = These Eps x
    That x <+> This Eps         = These Eps x
    That x <+> That y           = That (x <+> y)
    That x <+> These Eps y      = These Eps (x <+> y)
    These Eps x <+> These Eps y = These Eps (x <+> y)
    These Eps x <+> This Eps    = These Eps x
    These Eps x <+> That y      = These Eps (x <+> y)

instance (ChoiceSemigroup a) => Semigroup (EpsNFA a) where
    (ENFA aI aT aF) <> (ENFA bI bT bF) =
        let
            nbI = bI + IM.size aT
            nbT = shiftTransitionUp (IM.size aT) bT
            nF = shiftFinalUp (IM.size aT) bF
            nabT = IM.fromSet (const $ IM.singleton nbI $ This Eps) aF
            nT = nabT `unionTransition` aT `unionTransition` nbT
         in ENFA aI nT nF

instance ChoiceSemigroup a => ChoiceSemigroup (EpsNFA a) where
    (ENFA aI aT aF) <+> (ENFA bI bT bF) =
        let
            naI = aI + 1
            naF = shiftFinalUp 1 aF
            naT = shiftTransitionUp 1 aT
            nbI = bI + 1 + IM.size aT
            nbT = shiftTransitionUp (1 + IM.size aT) bT
            nbF = shiftFinalUp (1 + IM.size aT) bF
            nabT = IM.singleton 0 $ IM.singleton naI (This Eps) <> IM.singleton nbI (This Eps)
         in ENFA 0 (naT `unionTransition` nbT `unionTransition` nabT) (naF <> nbF)

computeClosures :: IntMap (IntMap (These Eps a)) -> IntMap IntSet
computeClosures tr = 
    let g = epsTransitionToGraph tr
     in IM.fromSet (IS.fromList . reachable g) $ IM.keysSet tr

epsTransitionToGraph :: IntMap (IntMap (These Eps a)) -> Graph
epsTransitionToGraph tr = 
    buildG (IS.findMin . IM.keysSet $ tr, IS.findMax . IM.keysSet $ tr) 
        [ (i, j) 
        | (i, trI) <- IM.toList tr
        , (j, act) <- IM.toList trI
        , not . isThat $ act ]

computeClosureTransition
    :: (ChoiceSemigroup a) 
    => IntMap (IntMap (These Eps a)) -> IntSet -> IntMap a
computeClosureTransition tr =
    foldl' (IM.unionWith (<+>)) IM.empty
    . map (IM.mapMaybe justThat)
    . toList . IM.restrictKeys tr

mnfaTransitionToEnfa :: IntMap (IntMap a) -> IntMap (IntMap (These Eps a))
mnfaTransitionToEnfa = IM.map (IM.map That)

enfaToMnfa :: ChoiceSemigroup a => EpsNFA a -> MagicNFA a
enfaToMnfa (ENFA i t f) =
    let toClosures = computeClosures t
        allClosures = toList . Set.fromList . toList $ toClosures
        nStates = length allClosures
        closureToNewState = Map.fromList (zip allClosures [0..nStates])
        oldToNew = IM.map (closureToNewState Map.!) toClosures
        newT = IM.fromList $ 
            zip [0..nStates] . map (IM.mapKeysWith (<+>) (oldToNew IM.!) 
            . computeClosureTransition t)
            $ allClosures
     in MNFA
            (oldToNew IM.! i)
            newT
            (IS.fromList 
                . map (closureToNewState Map.!) 
                . filter (not . IS.null . IS.intersection f) 
                $ allClosures)

mnfaToEnfa :: MagicNFA a -> EpsNFA a
mnfaToEnfa (MNFA i t f) = ENFA i (mnfaTransitionToEnfa t) f

instance ChoiceSemigroup a => Semigroup (MagicNFA a) where
    a <> b = enfaToMnfa (mnfaToEnfa a <> mnfaToEnfa b)

instance ChoiceSemigroup a => Monoid (MagicNFA a) where
    mempty = MNFA 0 IM.empty (IS.singleton 0)

instance ChoiceSemigroup a => ChoiceSemigroup (MagicNFA a) where
    a <+> b = enfaToMnfa (mnfaToEnfa a <+> mnfaToEnfa b)

instance (ChoiceSemigroup a, ParallelSemigroup a) =>  ParallelSemigroup (MagicNFA a) where
    p  <||> q = productWith (<||>) p q 

instance (ChoiceSemigroup a, OrderedSemigroup a) =>  OrderedSemigroup (MagicNFA a) where
    p  <.> q = productWith (<.>) p q 

instance (ChoiceSemigroup a, ParallelSemigroup a) =>  ParallelSemigroup (EpsNFA a) where
    a <||> b = mnfaToEnfa $ enfaToMnfa a <||> enfaToMnfa b

instance (ChoiceSemigroup a) => MonoidStar (MagicNFA a) where
    star (MNFA i t f) = enfaToMnfa $
        let ni = i + 1
            nf = shiftFinalUp 1 f
            nt = mnfaTransitionToEnfa (shiftTransitionUp 1 t)
                    `unionTransition` IM.singleton 0 (IM.singleton ni (This Eps))
                    `unionTransition` IM.fromSet (const $ IM.singleton 0 (This Eps)) nf
         in ENFA 0 nt (IS.singleton 0)

instance Pointed MagicNFA where
    point x = MNFA 0 (IM.fromList [(0, IM.singleton 1 x), (1, IM.empty)]) (IS.singleton 1)

instance Pointed EpsNFA where
    point x = ENFA 0 (IM.fromList [(0, IM.singleton 1 $ That x), (1, IM.empty)]) (IS.singleton 1)

stepFinalLeft :: (Int -> Int -> Int) -> Int -> IntSet -> IntSet -> IntMap (IntMap (These Eps a))
stepFinalLeft encode lFinal lF rS = IM.fromList
    [ (encode lf r, IM.singleton (encode lFinal r) (This Eps))
    | lf <- IS.toList lF, r <- IS.toList rS]

stepFinalRight :: (Int -> Int -> Int) -> IntSet -> Int -> IntSet -> IntMap (IntMap (These Eps a))
stepFinalRight encode lS rFinal rF = stepFinalLeft (flip encode) rFinal rF lS

productWith :: ChoiceSemigroup a => (a -> a -> a) -> MagicNFA a -> MagicNFA a -> MagicNFA a
productWith f (MNFA aI aT aF) (MNFA bI bT bF) =
    let aSize = IM.size aT
        aFinal = IM.size aT
        bFinal = IM.size bT
        encode x y = x + y * (aSize + 1) -- accounting for final
        bothT = productWithTransition f encode aT bT
        aFinalT = stepFinalLeft encode aFinal aF (IM.keysSet bT)
        bFinalT = stepFinalRight encode (IM.keysSet aT) bFinal bF
        onlyA = IM.mapKeysMonotonic (`encode` bFinal)
            . IM.map (IM.mapKeysMonotonic (`encode` bFinal)) $ aT
        onlyB = IM.mapKeysMonotonic (aFinal `encode`)
            . IM.map (IM.mapKeysMonotonic (`encode` bFinal)) $ aT
        newAF = IS.map (`encode` bFinal) aF
        newBF = IS.map (aFinal `encode`) bF
     in enfaToMnfa $ ENFA
            (encode aI bI)
            (mnfaTransitionToEnfa bothT
                `unionTransition` aFinalT
                `unionTransition` bFinalT
                `unionTransition` mnfaTransitionToEnfa onlyA
                `unionTransition` mnfaTransitionToEnfa onlyB)
            (newAF <>newBF)

productWithTransitionTo
    :: (a -> a -> a) -> (Int -> Int -> Int) -> IntMap a -> IntMap a -> IntMap a
productWithTransitionTo f encode atr btr = IM.fromList
    [ (encode aTo bTo, f aAct bAct )
    | (aTo, aAct) <- IM.toList atr
    , (bTo, bAct) <- IM.toList btr]

productWithTransition
    :: (a -> a -> a) -> (Int -> Int -> Int) 
    -> IntMap (IntMap a) -> IntMap (IntMap a) -> IntMap (IntMap a)
productWithTransition f encode aT bT = IM.fromList
    [ (encode aFrom bFrom, productWithTransitionTo f encode aTo bTo)
    | (aFrom, aTo) <- IM.toList aT
    , (bFrom, bTo) <- IM.toList bT
    ]

