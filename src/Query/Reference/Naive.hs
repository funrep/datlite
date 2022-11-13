module Query.Reference.Naive where

-- Reference implementation based on
-- https://fkettelhoit.github.io/bottom-up-datalog-js/docs/dl.html

import Control.Monad
import Data.Maybe
import Data.List
import Data.Char

type Ent = String

type Attr = String

type Val = String

type Name = String

type Var = String

type Expr = String

data Fact
  = Triple Ent Attr Val
  | RuleFact Name [Expr]
  deriving (Show, Eq)

data Rule
  = Rule Name [Var] [Clause]
  deriving (Show, Eq)

data Clause
  = Pattern Expr Attr Expr
  | RuleClause Name [Expr]
  deriving (Show, Eq)

referenceQ :: [Var] -> [Clause] -> [Fact] -> [Rule] -> [[(Var, Val)]]
referenceQ vars clauses facts rules = answerQuery (RuleClause "query" vars) facts (query:rules)
  where
    query = Rule "query" vars clauses

answerQuery :: Clause -> [Fact] -> [Rule] -> [[(Var, Val)]]
answerQuery clause facts rules = evalClause (buildDb facts rules) clause

buildDb :: [Fact] -> [Rule] -> [Fact]
buildDb facts rules =
  let newFacts = foldl applyRule facts rules
  in if length facts == length newFacts
    then facts
    else buildDb newFacts rules

applyRule :: [Fact] -> Rule -> [Fact]
applyRule facts rule = facts `union` ruleAsFacts facts rule

ruleAsFacts :: [Fact] -> Rule -> [Fact]
ruleAsFacts facts rule@(Rule name vars _) =
  let allBindings = generateBindings (facts) rule
  in map (substituteRule name vars) allBindings

substituteRule :: Name -> [Var] -> [(Var, Expr)] -> Fact
substituteRule name vars bindings = RuleFact name $ map (unifyVar bindings) vars

unifyVar :: [(Var, Expr)] -> Var -> Expr
unifyVar bindings var
  | isVariable var = maybe var id (lookup var bindings)
  | otherwise = var

isVariable :: String -> Bool
isVariable = all isUpper

generateBindings :: [Fact] -> Rule -> [[(Var, Expr)]]
generateBindings facts (Rule _ _ []) = []
generateBindings facts (Rule _ _ clauses)=
  let goals = map (evalClause facts) clauses
  in foldl unifyBindingArrays (head goals) (tail goals)

unifyBindingArrays :: [[(Var, Expr)]] -> [[(Var, Expr)]] -> [[(Var, Expr)]]
unifyBindingArrays arr1 arr2 = concat $
  map (\bindings -> catMaybes $ map (unifyBindings bindings) arr2) arr1

unifyBindings :: [(Var, Expr)] -> [(Var, Expr)] -> Maybe [(Var, Expr)]
unifyBindings bindings1 bindings2 =
  let joined1 = joinMap bindings1 bindings2
      joined2 = joinMap bindings2 bindings1
  in if sort joined1 == sort joined2
    then Just joined1
    else Nothing

joinMap :: Eq a => [(a, b)] -> [(a, b)] -> [(a, b)]
joinMap [] ds = ds
joinMap ((k, v):kvs) ds = (k, v) : joinMap kvs (filter (\(k', _) -> k /= k') ds)

evalClause :: [Fact] -> Clause -> [[(Var, Expr)]]
evalClause facts clause =
  let matchedFacts = filter (unify clause) facts
  in filter (not . null) $ map (asBinding clause) matchedFacts

unify :: Clause -> Fact -> Bool
unify (Pattern ent1 attr1 val1) (Triple ent2 attr2 val2) =
  all (\(k, v) -> k == v || isVariable k || isVariable v) $ zip [ent1, attr1, val1] [ent2, attr2, val2]
unify (RuleClause name1 exprs1) (RuleFact name2 exprs2) =
  all (\(k, v) -> k == v || isVariable k || isVariable v) $ zip (name1 : exprs1) (name2 : exprs2)
unify _ _ = False

asBinding :: Clause -> Fact -> [(Var, Expr)]
asBinding (Pattern ent1 attr1 val1) (Triple ent2 attr2 val2) =
  filter (\(k, _v) -> isVariable k) $ zip [ent1, attr1, val1] [ent2, attr2, val2]
asBinding (RuleClause name1 exprs1) (RuleFact name2 exprs2) =
  filter (\(k, _v) -> isVariable k) $ zip (name1 : exprs1) (name2 : exprs2)
asBinding _ _ = []
