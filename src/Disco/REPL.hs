-- REPL module

module Disco.REPL where

import Control.Monad.State

import qualified Data.Map as M

import System.IO

import Text.ParserCombinators.Parsec

import Disco.Env
import Disco.Runtime
import Disco.Parser

-----------
-- TYPES --
-----------

type REPL = StateT Runtime IO

--------------------
-- REPL FUNCTIONS --
--------------------

-- Run the disco REPL
runDiscoREPL :: REPL ()
runDiscoREPL = do
  io $ putStr "> "
  io $ hFlush stdout
  input <- io $ getLine
  case parse inputLine "" input of
    Left err -> io $ putStrLn $ "Parse error: " ++ show err
    Right expr -> do
      rt <- get
      let (res,env) = runState (eval expr) (globalEnv rt)
      put $ rt { globalEnv = env }
      case res of
        Left err' -> io $ putStrLn $ "Evaluation error: " ++ err'
        Right r -> io $ putStrLn $ printResult r
  --putStrLn . show $ parse inputLine "" input
  runDiscoREPL

-- Create the initial runtime
initRuntime :: Runtime
initRuntime = Runtime {
globalEnv = Env {
    bindings = M.empty,
    parentEnv = Just constants
  }
}

-- Lift an IO action into the REPL monad
io :: IO a -> REPL a
io = liftIO
