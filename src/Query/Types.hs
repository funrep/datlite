module Query.Types where

import Entity

type Name = String

type Var = String

data Expr = ExprVal Val | ExprVar Var
  deriving (Show, Eq)

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

type QueryEngine = [Var] -> [Clause] -> [Fact] -> [Rule] -> [[(Var, Val)]]
