{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module BellKAT.Implementations.AutomataExecution
    ( execute
    , executeHyper
    , evalExecution
    , runExecution
    , executeAutomata
    , getAllStates
    , showStates
    , printAutomatonStats
    , ExecutionParams(..)
    ) where

import           Data.IntMap.Strict (IntMap)
import           Data.Default
import qualified Data.IntMap.Strict as IM
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Foldable (toList)
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           Control.Monad.Except

import BellKAT.Implementations.Automata

execute :: (Ord s, Show s)
    => ExecutionParams s
    -> (a -> s -> Set s)
    -> MagicNFA a
    -> s -> Maybe (Set s)
execute params executeStep mnfa x =
    let err = evalExecution params executeStep mnfa x 
            $ executeAutomata >> getFinalStatesCombined
     in case err of
          Left _ -> Nothing
          Right final -> Just final

executeHyper :: (Ord s, Show s)
    => ExecutionParams s
    -> (a -> s -> Set s)
    -> HyperMagicNFA a
    -> s -> Maybe (Set s)
executeHyper params executeStep (HyperMagicNFA mnfa) = execute params executeStepSet mnfa
  where 
      executeStepSet as s = mconcat (map (`executeStep` s) $ toList as)

getAllStates :: Ord s => ExecutionMonad a s (IntMap (Set s))
getAllStates = gets esProcessed

getFinalStatesCombined :: Ord s => ExecutionMonad a s (Set s)
getFinalStatesCombined = do
    resFinal <- IM.restrictKeys <$> getAllStates <*> asks (mnfaFinal . eeAutomaton)
    pure $ IM.foldl' Set.union Set.empty resFinal

showStates :: Show s => MagicNFA a -> IntMap (Set s) -> String
showStates x = concatMap showState . IM.toList
  where
    showState (s, zs) = 
        if Set.null zs
           then ""
           else showStateId x s <> ":\n" 
            <> unlines (map (\z -> "  " <> show z) $ Set.toList zs)

evalExecution :: Ord s
    => ExecutionParams s
    -> (a -> s -> Set s)
    -> MagicNFA a
    -> s
    -> ExecutionMonad a s b
    -> Either ExecutionError b
evalExecution params executeStep mnfa x m = 
    let env = EE { eeAutomaton = mnfa, eeStepEvaluation = executeStep, eeExecutionParams = params }
     in (`evalState` initialState mnfa x) . runExceptT . (`runReaderT` env) $ m

runExecution :: Ord s
    => ExecutionParams s
    -> (a -> s -> Set s)
    -> MagicNFA a
    -> s
    -> ExecutionMonad a s b
    -> (Either ExecutionError b, ExecutionState s)
runExecution params executeStep mnfa x m =
    let env = EE { eeAutomaton = mnfa, eeStepEvaluation = executeStep, eeExecutionParams = params }
     in (`runState` initialState mnfa x) . runExceptT . (`runReaderT` env) $ m

initialState :: MagicNFA a -> s -> ExecutionState s
initialState mnfa x = ES
            { esPending = IM.singleton (mnfaInitial mnfa) (Set.singleton x)
            , esProcessed = IM.map (const Set.empty) (mnfaTransition mnfa)
            }

data ExecutionState s = ES
    { esPending   :: IntMap (Set s)
    , esProcessed :: IntMap (Set s)
    }

data ExecutionParams s = EP 
    { maxOptionsPerState :: Maybe Int
    , isValidState :: s -> Bool
    }

instance Default (ExecutionParams s) where
    def = EP Nothing (const True)

data ExecutionError = TooManyStates | InvalidStateReached

data ExecutionEnvironment a s = EE
    { eeAutomaton :: MagicNFA a
    , eeStepEvaluation :: a -> s -> Set s
    , eeExecutionParams :: ExecutionParams s
    }

type ExecutionMonad a s = ReaderT (ExecutionEnvironment a s) (ExceptT ExecutionError (State (ExecutionState s)))

executeAutomata :: (Ord s, Show s) => ExecutionMonad a s ()
executeAutomata =
    popNextPending >>= \case
        Nothing -> return ()
        Just (i, h) -> do
            markProcessed i h
            fromI <- reader ((IM.! i) . mnfaTransition . eeAutomaton)
            evalStep <- reader eeStepEvaluation
            forM_ (IM.toList fromI) $ \(j, x) -> appendStates j $ evalStep x h
            checkStateBound
            executeAutomata

checkStateBound :: ExecutionMonad a s ()
checkStateBound = reader (maxOptionsPerState . eeExecutionParams) >>= \case 
    Nothing -> pure ()
    Just maxOptions -> do
      curMaxOptions <- gets (IM.foldl' max 0 . IM.map Set.size . esProcessed)
      when (curMaxOptions > maxOptions) $ throwError TooManyStates

markProcessed :: (Ord s) => IM.Key -> s -> ExecutionMonad a s ()
markProcessed s hs = modify' $ \st -> 
    st { esProcessed = IM.insertWith Set.union s (Set.singleton hs) $ esProcessed st }

appendStates :: (Ord s) => IM.Key -> Set s -> ExecutionMonad a s ()
appendStates s hs = do
    checkState <- reader (isValidState . eeExecutionParams)
    unless (all checkState hs) $ throwError InvalidStateReached
    currentState <- gets ((IM.! s) . esProcessed)
    addPending s $ Set.difference hs currentState

addPending :: (Ord s) => IM.Key -> Set s -> ExecutionMonad a s ()
addPending s hs
  | Set.size hs == 0 = return ()
  | otherwise = modify' $ \st -> st { esPending = IM.insertWith Set.union s hs $ esPending st }

popNextPending :: ExecutionMonad a s (Maybe (Int, s))
popNextPending = do
    p <- gets esPending
    if IM.size p == 0
       then pure Nothing
       else do let ((k, v), p') = IM.deleteFindMin p
               let (r, v') = Set.deleteFindMin v
               modify' $ \st -> 
                   st { esPending = IM.union p' $ 
                       if Set.size v' == 0 then IM.empty else IM.singleton k v' }
               return $ Just (k, r)

printAutomatonStats :: (Monoid b, Ord b, Show b, Show a) => (a -> b -> Set b) -> MagicNFA a -> IO ()
printAutomatonStats exec nfa =
    let
        st = evalExecution def exec nfa mempty $ 
            executeAutomata >> getAllStates
    in case st of 
         Left _ -> putStrLn "execution error"
         Right states -> do
             let reachableStates = IM.filter (not . null) states
             putStrLn $ showStates nfa reachableStates
             print $ restrictStates nfa (IM.keysSet reachableStates)
