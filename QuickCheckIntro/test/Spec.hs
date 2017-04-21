{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Main where

import Arithmetic
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.QuickCheck


--------------------------------------------------------------------------------
-- Run the test suite
--------------------------------------------------------------------------------

main :: IO ()
main = do
  quickCheck prop_optimize_constant
  quickCheck prop_partial_constant
  quickCheck prop_optimize_eval
  quickCheck prop_dependencies_allow_eval
  quickCheck (expectFailure prop_missing_dependencies_forbid_eval)
  quickCheck (expectFailure prop_optimize_preserves_dependencies)


--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

instance Arbitrary Expr where
  arbitrary = sized genExpr

genCst :: Gen Expr
genCst = fmap cst arbitrary

varNames :: [String]
varNames = [[v] | v <- ['a'..'z']]

genVar :: Gen Expr
genVar = fmap var (elements varNames)

genSimpleTerm :: Gen Expr
genSimpleTerm = oneof [genVar, genCst]

opsGen :: Gen Expr -> Int -> Gen Expr
opsGen simpleTermGen = go where
  go n = do
    m <- choose (0, n)
    if m == 0
      then simpleTermGen
      else elements [add, mul] <*> replicateM m (go (div n (m + 1)))

genExpr :: Int -> Gen Expr
genExpr = opsGen genSimpleTerm

genCstExpr :: Int -> Gen Expr
genCstExpr = opsGen genCst

makeEnvWith :: Set.Set String -> Gen Env
makeEnvWith deps = do
  let n = Set.size deps
  values <- replicateM n arbitrary
  return $ makeEnv (zip (Set.toList deps) values)

genTotalEnv :: Gen Env
genTotalEnv = makeEnvWith (Set.fromList varNames)


--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_optimize_constant :: Property
prop_optimize_constant = forAll (sized genCstExpr) (isCst . optimize)

prop_partial_constant :: Property
prop_partial_constant = forAll (sized genCstExpr) (isCst . partial Map.empty)

prop_optimize_eval :: Expr -> Property
prop_optimize_eval e =
  forAll genTotalEnv $ \env ->
    eval env e == eval env (optimize e)

prop_dependencies_allow_eval :: Property
prop_dependencies_allow_eval =
  forAll (sized genExpr) $ \e ->
    forAll (makeEnvWith (dependencies e)) $ \env ->
      isCst (partial env e)
      && cst (eval env e) == partial env e

makePartialEnv :: Set.Set Id -> Gen Env
makePartialEnv deps = do
  v <- elements (Set.toList deps)
  makeEnvWith (Set.delete v deps)

prop_missing_dependencies_forbid_eval :: Property
prop_missing_dependencies_forbid_eval =
  forAll (sized genExpr) $ \e ->
    let deps = dependencies e
    in Set.size deps > 0 ==>
        forAll (makePartialEnv deps) $ \env ->
          not (isCst (partial env e))

prop_optimize_preserves_dependencies :: Property
prop_optimize_preserves_dependencies =
  forAll (sized genExpr) $ \e ->
    dependencies e == dependencies (optimize e)
