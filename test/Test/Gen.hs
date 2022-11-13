module Test.Gen where

import Test.QuickCheck
import Data.List

import Query.Types
import Entity

genQueryEngineInput :: Gen ([Var], [Clause], [Fact], [Rule])
genQueryEngineInput = do
  vars <- genVarsTopLevel
  rules <- genRules
  clauses <- listOf $ genRuleClauseTopLevel vars rules
  facts <- genFacts
  pure (vars, clauses, facts, rules)

genFacts :: Gen [Fact]
genFacts = do
  l <- chooseInt (1, 8)
  facts <- resize l $ listOf1 genFact
  pure facts

genFact :: Gen Fact
genFact = do
  ent <- genEnt
  Triple ent <$> genAttr <*> genVal ent

genEnt :: Gen Ent
genEnt = elements ["x", "y", "z"]

genAttr :: Gen Attr
genAttr = elements ["is", "has", "owes"]

genExpr :: Gen Expr
genExpr = oneof [genExprVal]

genExprVal :: Gen Expr
genExprVal = ExprVal <$> genVal ""

genVal :: Ent -> Gen Val
genVal ent = oneof [genEntVal ent]

genEntVal :: Ent -> Gen Val
genEntVal ent = EntVal <$> suchThat genEnt (/= ent)

genVarsTopLevel :: Gen [Var]
genVarsTopLevel = sublistOf ["A", "B", "C"]

vars :: [Var]
vars = ["D", "E", "F"]

genVars :: Gen [Var]
genVars = sublistOf vars

genVar :: Gen Var
genVar = elements vars

ruleNames :: [Name]
ruleNames = ["rule1", "rule2", "rule3"]

genName :: Gen Name
genName = elements ruleNames

genRules :: Gen [Rule]
genRules = do
  rules <- sequence [genRule "rule1" 1, genRule "rule2" 2, genRule "rule3" 3]
  rules' <- sublistOf rules
  if null rules'
    then pure $ take 1 rules 
    else pure $ rules'

genRule :: Name -> Int -> Gen Rule
genRule name nArgs = do
  vs <- take nArgs <$> shuffle vars
  l <- chooseInt (1, nArgs * 2)
  clauses <- resize l $ listOf1 $ genClause vs
  pure $ Rule name vars clauses

genClauseTopLevel :: [Var] -> [Rule] -> Gen Clause
genClauseTopLevel vars rules = oneof [genPattern vars, genRuleClauseTopLevel vars rules]

genClause :: [Var] -> Gen Clause
genClause vars = oneof [genPattern vars, genRuleClause vars]

genPattern :: [Var] -> Gen Clause
genPattern vars
  | length vars < 2 = do
    expr1 <- oneof $ genExpr : fmap (pure . ExprVar) vars
    attr <- genAttr
    expr2 <- genExpr
    pure $ Pattern expr1 attr expr2
  | otherwise = do
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
  case name of
    "rule1" -> pure $ RuleClause name $ take 1 exprs
    "rule2" -> pure $ RuleClause name $ take 2 exprs
    "rule3" -> pure $ RuleClause name $ take 3 exprs
    r -> error $ "No rule clause defined" ++ r
  where
    genExpr' = oneof $ genExpr : fmap (pure . ExprVar) vars

genRuleClauseTopLevel :: [Var] -> [Rule] -> Gen Clause
genRuleClauseTopLevel vars rules = do
  (Rule name vars _) <- elements rules
  exprs <- listOf1 genExpr'
  let nOfVars = length vars
  pure $ RuleClause name $ take nOfVars exprs
  where
    genExpr' = oneof $ genExpr : fmap (pure . ExprVar) vars
