-- Parser module

module Disco.Parser where

import Control.Applicative ((<$>))

import Numeric

import Text.ParserCombinators.Parsec

import Disco.Absyn

------------------------
-- LANGUAGE CONSTANTS --
------------------------

operators :: [String]
operators = ["+", "-", "*", "/", "=", ">"]

----------------------
-- PARSER FUNCTIONS --
----------------------

-- Parse a line of input
inputLine :: Parser Expr
inputLine = do
  whitespace
  expression

-- Parse an expression
expression :: Parser Expr
expression = do
  bq <- option "" $ return <$> char '`'
  whitespace
  res <- try sexpr <|> try number <|> try litstring <|> try character <|> symbol
  return $ if null bq then res else SExpr [Symbol "quote", res]

-- Parse an S-expression
sexpr :: Parser Expr
sexpr = do
  skip '('
  whitespace
  es <- expression `sepEndBy1` whitespace
  whitespace
  skip ')'
  return $ SExpr es

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

-- Parse a string literal
litstring :: Parser Expr
litstring = do
  skip '"'
  str <- anyChar `manyTill` skip '"'
  return $ LitString str

-- Parse a character literal
character :: Parser Expr
character = do
  skip '\''
  c <- anyChar
  skip '\''
  return $ Character c

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

-- Consume whitespace
whitespace :: Parser ()
whitespace = skipMany $ oneOf " \n\r\t"

-- Skip a character
skip :: Char -> Parser ()
skip c = do
  _ <- char c
  return ()
