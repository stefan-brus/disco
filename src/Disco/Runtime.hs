-- Runtime module

module Disco.Runtime where

import Control.Applicative
import Control.Monad.State

import qualified Data.Map as M

import Disco.Absyn
import Disco.Env

-----------
-- TYPES --
-----------

data Result =
    ResultNum NumberType
  | ResultBool Bool
  | ResultChar Char
  | ResultString String
  | ResultLookup String
  | ResultSExpr [Expr]
  deriving (Show)


type BuiltinFunc = [Expr] -> State Env (Either String Result)

data Runtime = Runtime {
  globalEnv :: Env
}

---------------
-- CONSTANTS --
---------------

-- Constants
constants :: Env
constants = M.fromList [
  ("true",Boolean True),
  ("false",Boolean False)
  ]

-- Map of builtin functions
builtins :: M.Map String BuiltinFunc
builtins = M.fromList [
  ("+",btinAdd),
  ("-",btinSub),
  ("*",btinMul),
  ("/",btinDiv),

  ("quote",btinQuote),
  ("eval",btinEval)
  ]

-------------------------
-- EVALUATOR FUNCTIONS --
-------------------------

-- Evaluate a list of expressions, stop if one fails
evalExprs :: [Expr] -> State Env (Either String [Result])
evalExprs = (sequence <$>) . mapM eval

-- Evaluate an expression
eval :: Expr -> State Env (Either String Result)
eval (Symbol s) = evalLookup s
eval (Number n) = return $ Right (ResultNum n)
eval (Boolean b) = return $ Right (ResultBool b)
eval (Character c) = return $ Right (ResultChar c)
eval (LitString s) = return $ Right (ResultString s)
eval (SExpr (fn:args)) = do
  res <- eval fn
  case res of
    Right (ResultLookup name) -> evalFunc name args
    Left err -> return $ Left err
    _ -> return . Left $ show fn ++ " is not a function."
eval e = return . Left $ "Cannot evaluate expression: " ++ show e

-- Evaluate a symbol lookup
evalLookup :: String -> State Env (Either String Result)
evalLookup name = case envGet constants name of
  Just e -> eval e
  Nothing -> return . Right $ ResultLookup name

-- Evaluate a function call
evalFunc :: String -> [Expr] -> State Env (Either String Result)
evalFunc name args = case M.lookup name builtins of
  Just fn -> fn args
  Nothing -> return . Left $ name ++ " is not a function."

-- Print the result of evaluating an expression
printResult :: Result -> String
printResult (ResultNum (NumberInt i)) = show i
printResult (ResultNum (NumberReal r)) = show r
printResult r = show r

-- Convert a result to an expression
resultToExpr :: Result -> Expr
resultToExpr (ResultNum n) = Number n
resultToExpr (ResultBool b) = Boolean b
resultToExpr (ResultChar c) = Character c
resultToExpr (ResultString s) = LitString s
resultToExpr (ResultLookup s) = Symbol s
resultToExpr (ResultSExpr es) = SExpr es

-----------------------
-- BUILTIN FUNCTIONS --
-----------------------

-- Built in add function '+'
btinAdd :: BuiltinFunc
btinAdd exprs = do
  res <- evalExprs exprs
  case res of
    Right [ResultNum n1, ResultNum n2] -> return . Right $ evalAdd n1 n2
    Right [_,_] -> return $ Left "+: arguments must be numbers"
    Right _ -> return $ Left "+: expects 2 arguments"
    Left err -> return $ Left err
  where
    evalAdd :: NumberType -> NumberType -> Result
    evalAdd (NumberInt i1) (NumberInt i2) = ResultNum . NumberInt $ i1 + i2
    evalAdd (NumberReal r1) (NumberReal r2) = ResultNum . NumberReal $ r1 + r2
    evalAdd (NumberInt i) (NumberReal r) = ResultNum . NumberReal $ fromInteger i + r
    evalAdd (NumberReal r) (NumberInt i) = ResultNum . NumberReal $ r + fromInteger i

-- Built in subtract function '-'
btinSub :: BuiltinFunc
btinSub exprs = do
  res <- evalExprs exprs
  case res of
    Right [ResultNum n1, ResultNum n2] -> return . Right $ evalSub n1 n2
    Right [_,_] -> return $ Left "-: arguments must be numbers"
    Right _ -> return $ Left "-: expects 2 arguments"
    Left err -> return $ Left err
  where
    evalSub :: NumberType -> NumberType -> Result
    evalSub (NumberInt i1) (NumberInt i2) = ResultNum . NumberInt $ i1 - i2
    evalSub (NumberReal r1) (NumberReal r2) = ResultNum . NumberReal $ r1 - r2
    evalSub (NumberInt i) (NumberReal r) = ResultNum . NumberReal $ fromInteger i - r
    evalSub (NumberReal r) (NumberInt i) = ResultNum . NumberReal $ r - fromInteger i

-- Built in multiply function '*'
btinMul :: BuiltinFunc
btinMul exprs = do
  res <- evalExprs exprs
  case res of
    Right [ResultNum n1, ResultNum n2] -> return . Right $ evalMul n1 n2
    Right [_,_] -> return $ Left "*: arguments must be numbers"
    Right _ -> return $ Left "*: expects 2 arguments"
    Left err -> return $ Left err
  where
    evalMul :: NumberType -> NumberType -> Result
    evalMul (NumberInt i1) (NumberInt i2) = ResultNum . NumberInt $ i1 * i2
    evalMul (NumberReal r1) (NumberReal r2) = ResultNum . NumberReal $ r1 * r2
    evalMul (NumberInt i) (NumberReal r) = ResultNum . NumberReal $ fromInteger i * r
    evalMul (NumberReal r) (NumberInt i) = ResultNum . NumberReal $ r * fromInteger i

-- Built in divide function '/'
btinDiv :: BuiltinFunc
btinDiv exprs = do
  res <- evalExprs exprs
  case res of
    Right [_, ResultNum (NumberInt 0)] -> return $ Left "/: division by zero"
    Right [ResultNum n1, ResultNum n2] -> return . Right $ evalDiv n1 n2
    Right [_,_] -> return $ Left "/: arguments must be numbers"
    Right _ -> return $ Left "/: expects 2 arguments"
    Left err -> return $ Left err
  where
    evalDiv :: NumberType -> NumberType -> Result
    evalDiv (NumberInt i1) (NumberInt i2) = ResultNum . NumberReal $ fromInteger i1 / fromInteger i2
    evalDiv (NumberReal r1) (NumberReal r2) = ResultNum . NumberReal $ r1 / r2
    evalDiv (NumberInt i) (NumberReal r) = ResultNum . NumberReal $ fromInteger i / r
    evalDiv (NumberReal r) (NumberInt i) = ResultNum . NumberReal $ r / fromInteger i

-- Bult in quote function, returns the expression as-is
btinQuote :: BuiltinFunc
btinQuote [Symbol s] = return . Right $ ResultLookup s
btinQuote [Number n] = return . Right $ ResultNum n
btinQuote [Boolean b] = return . Right $ ResultBool b
btinQuote [Character c] = return . Right $ ResultChar c
btinQuote [LitString s] = return . Right $ ResultString s
btinQuote [SExpr exprs] = return . Right $ ResultSExpr exprs
btinQuote e | length e == 1 = return . Left $ "quote: unknown expression: " ++ show (head e)
btinQuote _ = return $ Left "quote: expects 1 argument"

-- Bult in eval function, evaluate the result of evaluating the argument
btinEval :: BuiltinFunc
btinEval [e] = do
  res <- eval e
  case res of
    Right r -> eval $ resultToExpr r
    Left err -> return $ Left err
btinEval _ = return $ Left "eval: expects 1 argument"
