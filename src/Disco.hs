-- Disco language module

module Disco where

import Control.Monad.State

import Disco.REPL

-- Launch the Disco REPL
runDisco :: IO ()
runDisco = evalStateT runDiscoREPL initRuntime
