module Test.Gen where

import Test.QuickCheck
import Data.List

import Query.Types
import Entity

genQueryEngineInput :: Gen ([Var], [Clause], [Fact], [Rule])
genQueryEngineInput = do
  vars <- listOf genVar
  clauses <- listOf $ genClause vars
  facts <- listOf genFact
  rules <- listOf genRule
  pure (vars, clauses, facts, rules)

genFact :: Gen Fact
genFact = Triple <$> genEnt <*> genAttr <*> genVal

genEnt :: Gen Ent
genEnt = elements ["x", "y", "z"]

genAttr :: Gen Attr
genAttr = elements ["is", "has", "owes"]

genExpr :: Gen Expr
genExpr = oneof [genExprVal, genExprVar]

genExprVal :: Gen Expr
genExprVal = ExprVal <$> genVal

genExprVar :: Gen Expr
genExprVar = ExprVar <$> elements ["d", "e"]

genVal :: Gen Val
genVal = oneof [genStrVal, genEntVal]

genStrVal :: Gen Val
genStrVal = StrVal <$> elements ["pizza", "bob", "cheese"]

genEntVal :: Gen Val
genEntVal = EntVal <$> genEnt

genVar :: Gen Var
genVar = elements ["a", "b", "c"]

genName :: Gen Name
genName = elements ["f", "g", "h"]

genRule :: Gen Rule
genRule = do
  name <- genName
  vars <- listOf1 genVar
  clauses <- listOf1 (genClause vars)
  pure $ Rule name vars clauses

genClause :: [Var] -> Gen Clause
genClause vars = oneof [genPattern vars, genRuleClause vars]

genPattern :: [Var] -> Gen Clause
genPattern vars = do
  var1 <- elements vars
  expr1 <- oneof [pure (ExprVar var1), genExpr]
  attr <- genAttr
  var2 <- elements $ delete var1 vars
  expr2 <- oneof [pure (ExprVar var2), genExpr]
  pure $ Pattern expr1 attr expr2

genRuleClause :: [Var] -> Gen Clause
genRuleClause vars = do
  name <- genName
  exprs <- listOf1 genExpr'
  pure $ RuleClause name exprs
  where
    genExpr' = oneof [ExprVar <$> elements vars, genExpr]
