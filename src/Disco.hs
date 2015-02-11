-- Disco language module

module Disco where

import Control.Applicative ((<$>))

import qualified Data.Map as M

import Numeric

import System.IO

import Text.ParserCombinators.Parsec

-----------
-- TYPES --
-----------

data Expr =
    Symbol String
  | Number NumberType
  | SExpr [Expr]
  deriving (Show)

data NumberType =
    NumberInt Integer
  | NumberReal Double
  deriving (Show)

data Result =
    ResultNum NumberType
  | ResultLookup String
  deriving (Show)

type BuiltinFunc = [Expr] -> Either String Result

------------------------
-- LANGUAGE CONSTANTS --
------------------------

operators :: [String]
operators = ["+", "-", "*", "/"]

-----------------------
-- BUILTIN FUNCTIONS --
-----------------------

-- Map of builtin functions
builtins :: M.Map String BuiltinFunc
builtins = M.fromList [
  ("+",btinAdd),
  ("-",btinSub),
  ("*",btinMul),
  ("/",btinDiv)
  ]

-- Built in add function '+'
btinAdd :: BuiltinFunc
btinAdd exprs = case evalExprs exprs of
  Right args@[ResultNum n1, ResultNum n2] -> Right $ evalAdd n1 n2
  Right [x1,x2] -> Left "+: arguments must be numbers"
  Right xs -> Left "+: expects 2 arguments"
  Left err -> Left err
  where
    evalAdd :: NumberType -> NumberType -> Result
    evalAdd (NumberInt i1) (NumberInt i2) = ResultNum . NumberInt $ i1 + i2
    evalAdd (NumberReal r1) (NumberReal r2) = ResultNum . NumberReal $ r1 + r2
    evalAdd (NumberInt i) (NumberReal r) = ResultNum . NumberReal $ fromInteger i + r
    evalAdd (NumberReal r) (NumberInt i) = ResultNum . NumberReal $ r + fromInteger i

-- Built in subtract function '-'
btinSub :: BuiltinFunc
btinSub exprs = case evalExprs exprs of
  Right args@[ResultNum n1, ResultNum n2] -> Right $ evalSub n1 n2
  Right [x1,x2] -> Left "-: arguments must be numbers"
  Right xs -> Left "-: expects 2 arguments"
  Left err -> Left err
  where
    evalSub :: NumberType -> NumberType -> Result
    evalSub (NumberInt i1) (NumberInt i2) = ResultNum . NumberInt $ i1 - i2
    evalSub (NumberReal r1) (NumberReal r2) = ResultNum . NumberReal $ r1 - r2
    evalSub (NumberInt i) (NumberReal r) = ResultNum . NumberReal $ fromInteger i - r
    evalSub (NumberReal r) (NumberInt i) = ResultNum . NumberReal $ r - fromInteger i

-- Built in multiply function '*'
btinMul :: BuiltinFunc
btinMul exprs = case evalExprs exprs of
  Right args@[ResultNum n1, ResultNum n2] -> Right $ evalMul n1 n2
  Right [x1,x2] -> Left "*: arguments must be numbers"
  Right xs -> Left "*: expects 2 arguments"
  Left err -> Left err
  where
    evalMul :: NumberType -> NumberType -> Result
    evalMul (NumberInt i1) (NumberInt i2) = ResultNum . NumberInt $ i1 * i2
    evalMul (NumberReal r1) (NumberReal r2) = ResultNum . NumberReal $ r1 * r2
    evalMul (NumberInt i) (NumberReal r) = ResultNum . NumberReal $ fromInteger i * r
    evalMul (NumberReal r) (NumberInt i) = ResultNum . NumberReal $ r * fromInteger i

-- Built in divide function '/'
btinDiv :: BuiltinFunc
btinDiv exprs = case evalExprs exprs of
  Right [_, ResultNum (NumberInt 0)] -> Left "/: division by zero"
  Right args@[ResultNum n1, ResultNum n2] -> Right $ evalDiv n1 n2
  Right [x1,x2] -> Left "/: arguments must be numbers"
  Right xs -> Left "/: expects 2 arguments"
  Left err -> Left err
  where
    evalDiv :: NumberType -> NumberType -> Result
    evalDiv (NumberInt i1) (NumberInt i2) = ResultNum . NumberReal $ fromInteger i1 / fromInteger i2
    evalDiv (NumberReal r1) (NumberReal r2) = ResultNum . NumberReal $ r1 / r2
    evalDiv (NumberInt i) (NumberReal r) = ResultNum . NumberReal $ fromInteger i / r
    evalDiv (NumberReal r) (NumberInt i) = ResultNum . NumberReal $ r / fromInteger i

-------------------------
-- EVALUATOR FUNCTIONS --
-------------------------

-- Evaluate a list of expressions, stop if one fails
evalExprs :: [Expr] -> Either String [Result]
evalExprs = mapM eval

-- Evaluate an expression
eval :: Expr -> Either String Result
eval (Symbol s) = Right (ResultLookup s)
eval (Number n) = Right (ResultNum n)
eval (SExpr (fn:args)) = case eval fn of
  Right (ResultLookup fn) -> evalFunc fn args
  _ -> Left $ show fn ++ " is not a function."

-- Evaluate a function call
evalFunc :: String -> [Expr] -> Either String Result
evalFunc name args = case M.lookup name builtins of
  Just fn -> fn args
  Nothing -> Left $ name ++ " is not a function."

-- Print the result of evaluating an expression
printResult :: Result -> String
printResult (ResultNum (NumberInt i)) = show i
printResult (ResultNum (NumberReal r)) = show r
printResult r = show r

----------------------
-- PARSER FUNCTIONS --
----------------------

-- Parse a line of input
inputLine :: Parser Expr
inputLine = do
  whitespace
  expr

-- Parse an expression
expr :: Parser Expr
expr = try sexpr <|> try number <|> symbol

-- Parse an S-expression
sexpr :: Parser Expr
sexpr = do
  skip '('
  whitespace
  es <- expr `sepEndBy1` whitespace
  whitespace
  skip ')'
  return $ SExpr es

-- Parse a symbol
symbol :: Parser Expr
symbol = operator <|> identifier

-- Parse an operator
operator :: Parser Expr
operator = do
  op <- choice $ map string operators
  return $ Symbol op

-- Parse an identifier
identifier :: Parser Expr
identifier = do
  c <- letter
  rest <- many alphaNum
  return $ Symbol (c:rest)

-- Parse a number
number :: Parser Expr
number = do
  neg <- option "" $ return <$> char '-'
  int <- many1 digit
  com <- option "" $ return <$> char '.'
  dec <- if null com then return "" else many1 digit
  return $ mkNumber (null com) . head $ fst <$> (readSigned readFloat $ neg ++ int ++ com ++ dec)
  where
    mkNumber True x = Number . NumberInt $ floor x
    mkNumber False x = Number $ NumberReal x

-- Consume whitespace
whitespace :: Parser ()
whitespace = skipMany $ oneOf " \n\r\t"

-- Skip a character
skip :: Char -> Parser ()
skip c = do
  _ <- char c
  return ()

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
