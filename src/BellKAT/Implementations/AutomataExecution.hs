{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module BellKAT.Implementations.AutomataExecution 
    ( execute
    , defaultExecutionParams
    , ExecutionParams(..)
    ) where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           Control.Monad.Except

import BellKAT.Implementations.Automata

execute :: Ord s
    => ExecutionParams
    -> (a -> s -> Set s)
    -> MagicNFA a
    -> s -> Maybe (Set s)
execute params executeStep mnfa x = 
    let env = EE { eeAutomaton = mnfa, eeStepEvaluation = executeStep, eeExecutionParams = params }
        stInit = ES 
            { esPending = IM.singleton (mnfaInitial mnfa) (Set.singleton x) 
            , esProcessed = IM.map (const Set.empty) (mnfaTransition mnfa) 
            }
        (err, st) = (`runState` stInit) . runExceptT .(`runReaderT` env) $ executeAutomata
        resFinal = IM.restrictKeys (esProcessed st) (mnfaFinal mnfa)
     in case err of 
          Left _ -> Nothing
          Right () -> Just $ IM.foldl' Set.union Set.empty resFinal

data ExecutionState s = ES
    { esPending   :: IntMap (Set s)
    , esProcessed :: IntMap (Set s)
    }

newtype ExecutionParams = EP 
    { maxOptionsPerState :: Maybe Int
    }

defaultExecutionParams :: ExecutionParams
defaultExecutionParams = EP Nothing

data ExecutionError = TooManyStates

data ExecutionEnvironment a s = EE
    { eeAutomaton :: MagicNFA a
    , eeStepEvaluation :: a -> s -> Set s
    , eeExecutionParams :: ExecutionParams
    }

type ExecutionMonad a s = ReaderT (ExecutionEnvironment a s) (ExceptT ExecutionError (State (ExecutionState s)))

executeAutomata :: (Ord s) => ExecutionMonad a s ()
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
