{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Arithmetic where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set


type Id = String
type Env = Map.Map Id Int
data OpType
  = Add
  | Mul
  deriving (Show, Eq, Ord)

data ExprR r
  = Cst Int
  | Var Id
  | Op OpType [r]
  deriving (Show, Eq, Ord, Foldable, Traversable)

newtype Fix f = Fix { unFix :: f (Fix f) } -- f = f f
type Expr = Fix ExprR -- Fix point of ExprR

instance Eq (f (Fix f)) => Eq (Fix f) where
  a == b = unFix a == unFix b

instance Show Expr where
  show = prn

instance Functor ExprR where
  fmap _ (Cst c) = Cst c
  fmap _ (Var v) = Var v
  fmap f (Op opType xs) = Op opType (map f xs)

-- Smart constructors

cst = Fix . Cst
var = Fix . Var
add = Fix . Op Add
mul = Fix . Op Mul

isCst (Fix (Cst _)) = True
isCst _ = False

makeEnv :: [(Id, Int)] -> Env
makeEnv = Map.fromList

-- Catamorphism

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata algebra =
  algebra
  . fmap (cata algebra)
  . unFix


-- Composition

type Algebra f = f (Fix f) -> Fix f

comp :: Algebra f -> Algebra f -> Algebra f
comp f g = f . unFix . g

compAll :: Foldable t => t (Algebra f) -> Algebra f
compAll fs = foldr1 comp fs

-- Interpreters

eval :: Env -> Expr -> Int
eval env = cata algebra where
  algebra (Cst n) = n
  algebra (Var x) = env Map.! x
  algebra (Op Add xs) = sum xs
  algebra (Op Mul xs) = product xs

prn :: Expr -> String
prn = cata algebra where
  algebra (Cst n) = show n
  algebra (Var x) = x
  algebra (Op Add xs) = "(+ " ++ unwords xs ++ ")"
  algebra (Op Mul xs) = "(* " ++ unwords xs ++ ")"

-- Optimizers (to combine)

optimizeOp :: ExprR Expr -> Int -> (Int -> Int -> Int) -> Expr
optimizeOp (Op optType xs) neutral combine =
  let (constants, vars) = partition isCst xs
      constantsVal = map (\(Fix (Cst x)) -> x) constants
      sumCst = foldl' combine neutral constantsVal
  in case vars of
      []  -> cst sumCst
      [y] | sumCst == neutral -> y
      ys  | sumCst == neutral -> Fix (Op optType ys)
      ys  -> Fix (Op optType (cst sumCst : ys))

optimizeAdd :: ExprR Expr -> Expr
optimizeAdd op@(Op Add _) = optimizeOp op 0 (+)
optimizeAdd e = Fix e

optimizeMul :: ExprR Expr -> Expr
optimizeMul op@(Op Mul xs)
  | not (null (dropWhile (/= cst 0) xs)) = cst 0
  | otherwise = optimizeOp op 1 (*)
optimizeMul e = Fix e

-- Optimizer

optimize :: Expr -> Expr
optimize = cata (optimizeMul `comp` optimizeAdd)

-- Partial evals

replaceKnownVars :: Env -> ExprR Expr -> Expr
replaceKnownVars env = go where
  go e@(Var v) =
    case Map.lookup v env of
      Just val -> cst val
      Nothing -> Fix e
  go e = Fix e

partial :: Env -> Expr -> Expr
partial env = cata (compAll [optimizeMul, optimizeAdd, replaceKnownVars env])

-- Collector of variables

dependencies :: Expr -> Set.Set Id
dependencies = cata algebra where
  algebra (Cst _) = Set.empty
  algebra (Var x) = Set.singleton x
  algebra (Op _ xs) = Set.unions xs

-- Eval 2.0 (based on the fixing)

eval' :: Env -> Expr -> Int
eval' env expr =
  case partial env expr of
    (Fix (Cst n)) -> n
    e -> error $ "Missing vars: " ++ show (dependencies e)

--
