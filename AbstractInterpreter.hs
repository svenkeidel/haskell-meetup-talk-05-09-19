{-# LANGUAGE LambdaCase #-}
module AbstractInterpreter where

import Data.Map (Map)
import qualified Data.Map as M

data Expr = Var String | NumLit Int | Add Expr Expr | Mul Expr Expr

type Env = Map String Val
type Val = (Int,Int)

eval :: Env -> Expr -> Maybe Val
eval env = \case
  Var x -> M.lookup x env
  NumLit n -> return (n,n)
  Add e1 e2 -> do
    (i1,j1) <- eval env e1
    (i2,j2) <- eval env e2
    return (i1 + i2, j1 + j2)
  Mul e1 e2 -> do
    (i1,j1) <- eval env e1
    (i2,j2) <- eval env e2
    -- (-1,2) * (1,2)
    return (minimum [i1 * i2, i1 * i2, j1 * i2, j1 * j2], maximum [ i1 * i2, i1 * i2, j1 * i2, j1 * j2])
