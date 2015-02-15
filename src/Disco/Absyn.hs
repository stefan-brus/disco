-- Abstract syntax tree module

module Disco.Absyn where

-----------
-- TYPES --
-----------

data Expr =
    Symbol String
  | Number NumberType
  | Boolean Bool
  | Character Char
  | LitString String
  | SExpr [Expr]
  deriving (Show,Eq)

data NumberType =
    NumberInt Integer
  | NumberReal Double
  deriving (Show,Eq)

--------------------------
-- EXPRESSION FUNCTIONS --
--------------------------

-- Print an expression
printExpr :: Expr -> String
printExpr (Symbol s) = s
printExpr (Number (NumberInt i)) = show i
printExpr (Number (NumberReal r)) = show r
printExpr (Boolean True) = "true"
printExpr (Boolean False) = "false"
printExpr (Character c) = "'" ++ [c] ++ "'"
printExpr (LitString s) = "\"" ++ s ++ "\""
printExpr (SExpr xs) = "(" ++ (unwords $ map printExpr xs) ++ ")"
