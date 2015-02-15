-- Environment module

module Disco.Env where

import qualified Data.Map as M

import Disco.Absyn

-----------
-- TYPES --
-----------

type Bindings = M.Map String Expr

data Env = Env {
  bindings :: Bindings,
  parentEnv :: Maybe Env
}

-----------------
-- ENVIRONMENT --
-----------------

-- Look something up in the environment
envGet :: Env -> String -> Maybe Expr
envGet Env { bindings = b, parentEnv = p } name = case M.lookup name b of
  Just res -> Just res
  Nothing -> case p of
    Just env -> envGet env name
    Nothing -> Nothing

-- Bind a name in the environment
envSet :: Env -> String -> Expr -> Env
envSet env@(Env { bindings = b }) name expr = env { bindings = M.insert name expr b }
