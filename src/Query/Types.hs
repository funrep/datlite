module Query.Types where

import Entity

type Name = String

type Var = String

data Expr = ExprVal Val | ExprVar Var
  deriving (Show, Eq, Ord)

data Rule
  = Rule Name [Var] [Clause]
  deriving (Show, Eq)

data Clause
  = Pattern Expr Attr Expr
  | RuleClause Name [Expr]
  deriving (Show, Eq)

data DerivedFact
  = RuleFact Name [Expr]
  deriving (Show, Eq)

instance Ord DerivedFact where
  compare (RuleFact name1 exprs1) (RuleFact name2 exprs2)
    | name1 == name2 = compare exprs1 exprs2
    | otherwise = compare name1 name2

type QueryEngine = [Var] -> [Clause] -> [Fact] -> [Rule] -> [[(Var, Val)]]
