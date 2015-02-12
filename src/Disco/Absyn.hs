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
  deriving (Show)

data NumberType =
    NumberInt Integer
  | NumberReal Double
  deriving (Show)
