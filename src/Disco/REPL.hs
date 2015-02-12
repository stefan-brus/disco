-- REPL module

module Disco.REPL where

import System.IO

import Text.ParserCombinators.Parsec

import Disco.Runtime
import Disco.Parser

--------------------
-- REPL FUNCTIONS --
--------------------

-- Run the disco REPL
runDiscoREPL :: IO ()
runDiscoREPL = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case parse inputLine "" input of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right expr -> case eval expr of
      Left err' -> putStrLn $ "Evaluation error: " ++ err'
      Right res -> putStrLn $ printResult res
  --putStrLn . show $ parse inputLine "" input
  runDiscoREPL
