{-# LANGUAGE LambdaCase #-}
module ConcreteInterpreter where

import Data.Map(Map)
import qualified Data.Map as M
data Expr = Var String | NumLit Int | Add Expr Expr | Mul Expr Expr

type Val = Int
type Env = Map String Val

eval :: Env -> Expr -> Maybe Val
eval env = \case
  Var x -> M.lookup x env
  NumLit n -> Just n
  Add e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    return (v1 + v2)
  Mul e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    return (v1 * v2)
