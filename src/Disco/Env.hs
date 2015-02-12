-- Environment module

module Disco.Env where

import qualified Data.Map as M

import Disco.Absyn

-----------
-- TYPES --
-----------

type Env = M.Map String Expr

-----------------
-- ENVIRONMENT --
-----------------

-- Look something up in the environment
envGet :: Env -> String -> Maybe Expr
envGet e = flip M.lookup $ e
