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
constants = Env {
  bindings = M.fromList [
    ("true",Boolean True),
    ("false",Boolean False)
    ],
  parentEnv = Nothing
}

-- Map of builtin functions
builtins :: M.Map String BuiltinFunc
builtins = M.fromList [
  ("quote",btinQuote),
  ("eval",btinEval),
  ("set",btinSet),

  ("+",btinAdd),
  ("-",btinSub),
  ("*",btinMul),
  ("/",btinDiv),

  ("not",btinNot),
  ("=",btinEq),
  (">",btinGt)
  ]

-------------------------
-- EVALUATOR FUNCTIONS --
-------------------------

-- Evaluate a list of expressions, stop if one fails
evalExprs :: [Expr] -> State Env (Either String [Result])
evalExprs = (sequence <$>) . mapM eval

-- Evaluate an expression
eval :: Expr -> State Env (Either String Result)
eval expr = do
  env <- get
  put $ Env { bindings = M.empty, parentEnv = Just env }
  res <- case expr of
    Symbol s -> evalLookup s
    Number n -> return $ Right (ResultNum n)
    Boolean b -> return $ Right (ResultBool b)
    Character c -> return $ Right (ResultChar c)
    LitString s -> return $ Right (ResultString s)
    SExpr (fn:args) -> do
      res <- eval fn
      case res of
        Right (ResultLookup name) -> evalFunc name args
        Left err -> return $ Left err
        _ -> return . Left $ show fn ++ " is not a function"
    _ -> return . Left $ "Cannot evaluate expression: " ++ show expr
  penv <- gets parentEnv
  case penv of
    Just env' -> do
      put env'
      return res
    Nothing -> return . Left $ "Environment somehow disappeared for expression: " ++ show expr

-- Evaluate a symbol lookup
evalLookup :: String -> State Env (Either String Result)
evalLookup name = do
  env <- get
  case envGet env name of
    Just e -> eval e
    Nothing -> return . Right $ ResultLookup name

-- Evaluate a function call
evalFunc :: String -> [Expr] -> State Env (Either String Result)
evalFunc name args = case M.lookup name builtins of
  Just fn -> fn args
  Nothing -> return . Left $ name ++ " is not a function"

-- Print the result of evaluating an expression
printResult :: Result -> String
printResult (ResultNum (NumberInt i)) = show i
printResult (ResultNum (NumberReal r)) = show r
printResult (ResultBool True) = "true"
printResult (ResultBool False) = "false"
printResult (ResultChar c) = "'" ++ [c] ++ "'"
printResult (ResultString s) = "\"" ++ s ++ "\""
printResult (ResultLookup s) = s
printResult (ResultSExpr xs) = printExpr $ SExpr xs

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

--------------
-- LANGUAGE --
--------------

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

-- Built in set function, bind a name to an expression in the parent environment
btinSet :: BuiltinFunc
btinSet [name, expr] = case name of
  Symbol s -> do
    case envGet constants s of
      Just _ -> return . Left $ "set: symbol " ++ s ++ " is a constant"
      Nothing -> do
        env <- get
        case parentEnv env of
          Just penv -> do
            let res = envSet penv s expr
            put $ env { parentEnv = Just res }
            return . Right $ ResultLookup s
          Nothing -> return $ Left "set: no parent environment"
  _ -> return $ Left "set: first argument must be a symbol"
btinSet _ = return $ Left "set: expects 2 arguments"

----------
-- MATH --
----------

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

-------------
-- BOOLEAN --
-------------

-- Built in 'not' function
btinNot :: BuiltinFunc
btinNot [expr] = do
  res <- eval expr
  case res of
    Right (ResultBool b) -> return . Right $ ResultBool (not b)
    Right _ -> return $ Left "not: first argument must be boolean"
    Left err -> return $ Left err
btinNot _ = return $ Left "not: expects 1 argument"

-- Built in equality function, incompatible types result in error
btinEq :: BuiltinFunc
btinEq [e1,e2] = do
  r1 <- eval e1
  r2 <- eval e2
  case (r1,r2) of
    (Left err, _) -> return $ Left err
    (_, Left err) -> return $ Left err
    (Right res1, Right res2) -> case (res1,res2) of
      (ResultNum (NumberInt n1), ResultNum (NumberInt n2)) -> return . Right . ResultBool $ n1 == n2
      (ResultNum (NumberReal n1), ResultNum (NumberReal n2)) -> return . Right . ResultBool $ n1 == n2
      (ResultBool b1, ResultBool b2) -> return . Right . ResultBool $ b1 == b2
      (ResultChar c1, ResultChar c2) -> return . Right . ResultBool $ c1 == c2
      (ResultString s1, ResultString s2) -> return . Right . ResultBool $ s1 == s2
      (ResultSExpr x1, ResultSExpr x2) -> return . Right . ResultBool $ and $ zipWith (==) x1 x2
      _ -> return $ Left "=: incompatible types"
btinEq _ = return $ Left "=: expects 2 arguments"

-- Built in greater-than function, can only compare numbers
btinGt :: BuiltinFunc
btinGt [e1,e2] = do
  res1 <- eval e1
  res2 <- eval e2
  case (res1,res2) of
    (Left err, _) -> return $ Left err
    (_, Left err) -> return $ Left err
    (Right (ResultNum n1), Right (ResultNum n2)) -> case (n1,n2) of
      (NumberInt i1, NumberInt i2) -> return . Right . ResultBool $ i1 > i2
      (NumberReal r1, NumberReal r2) -> return . Right . ResultBool $ r1 > r2
      (NumberInt i, NumberReal r) -> return . Right . ResultBool $ fromInteger i > r
      (NumberReal r, NumberInt i) -> return . Right . ResultBool $ r > fromInteger i
    (_, _) -> return $ Left ">: arguments must be numbers"
btinGt _ = return $ Left ">: expects 2 arguments"
