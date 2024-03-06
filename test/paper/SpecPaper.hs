{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module SpecPaper where

import           Data.Functor.Compose         (Compose (..))
import qualified Data.IntMap.Strict as IM

import BellKAT.Drawing
import BellKAT.DSL
import BellKAT.PolicyEmbeddings
import BellKAT.Definitions hiding (test, (<.>))
import qualified BellKAT.Implementations.AutomataExecution as AE
import qualified BellKAT.Implementations.Automata as A
import qualified BellKAT.Implementations.AutomataStepHistoryQuantum    as ASHQ
import qualified BellKAT.Implementations.OneStepHistoryQuantum as OSHQ

type PaperPolicy = Ordered StarPolicy FreeTest (Maybe ())

main :: IO ()
main =
    let pd :: PaperPolicy  = 
            (create "C" <||> create "C") <>
                (trans "C" ("A", "D") <||> trans "C" ("A", "D")) <>
                    distill ("A", "D")
        bpd :: PaperPolicy = 
            (test ("A" /~? "D") <.> create "C" <||> test ("A" /~? "D") <.> create "C") <>
                (trans "C" ("A", "D") <||> trans "C" ("A", "D")) <>
                    distill ("A", "D")
        pd' :: PaperPolicy = 
            (create "E" <||> create "E") <> 
                (trans "E" ("E", "D") <||> trans "E" ("E", "D")) <>
                    distill ("E", "D")
        bpd' :: PaperPolicy = 
            (test ("E" /~? "D") <.> create "E" <||> test ("E" /~? "D") <.> create "E") <>
                (trans "E" ("E", "D") <||> trans "E" ("E", "D")) <>
                    distill ("E", "D")
        -- pad = pd <> star bpd <> test ("A" ~~? "D")
        _pd :: PaperPolicy = ucreate ("A", "D")
        _pd' :: PaperPolicy = ucreate ("E", "D")
        _bpd :: PaperPolicy = test ("A" /~? "D") <.> ucreate ("A", "D")
        _bpd' :: PaperPolicy = test ("E" /~? "D") <.> ucreate ("E", "D")
        ped = _pd' <> (mempty <+> _bpd') <> test ("E" ~~? "D")
        pad = _pd <> (mempty <+> _bpd) <> test ("A" ~~? "D")
        -- ped = pd' <> test ("E" ~~? "D")
        p =  (pad <||> ped) <> swap "D" ("A", "E")
        nfa = ASHQ.getNFA $ meaning @_ @(ASHQ.AutomatonStepHistoryQuantum (Compose OSHQ.OneStepPolicy (OSHQ.OneStepFree _) _)) (pad <||> ped)
        st = AE.evalExecution (AE.EP Nothing) OSHQ.executeFree nfa [] $ 
            AE.executeAutomata >> AE.getAllStates
    in do --putStrLn $ drawHistoriesText $ applyStarOrderedPolicy @(Maybe ()) (pad Def.<.> ped) []
       -- print (pad <||> ped)
       -- putStrLn $ drawHistoriesText $ applyStarOrderedPolicy @(Maybe ()) (pad <||> ped) []
       case st of 
         Left _ -> putStrLn "error"
         Right states -> do
             let reachableStates = IM.filter (not . null) states
             putStrLn $ AE.showStates nfa reachableStates
             print $ A.restrictStates nfa (IM.keysSet reachableStates)
