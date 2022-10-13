module Query.SemiNaive where

import Control.Monad
import Data.Maybe
import Data.List
import Data.Char

import Entity
import Query.Types

semiNaiveQ :: QueryEngine
semiNaiveQ vars clauses facts rules =
  generateBindings facts (derive facts rules) clauses

derive :: [Fact] -> [Rule] -> [DerivedFact]
derive facts rules =
  let derived = foldl (applyRule facts) [] $ incrementalRules rules
  in deriveRec facts derived derived $ restOfRules rules

deriveRec :: [Fact] -> [DerivedFact] -> [DerivedFact] -> [Rule] -> [DerivedFact]
deriveRec facts derived allDerived rules =
  let newDerived = foldl (applyRule facts) derived rules
  in if null newDerived
    then allDerived
    else deriveRec facts (newDerived \\ derived) (allDerived `union` newDerived) rules

restOfRules :: [Rule] -> [Rule]
restOfRules rules = filter (not . isIncremental) rules

incrementalRules :: [Rule] -> [Rule]
incrementalRules rules = filter isIncremental rules

isIncremental :: Rule -> Bool
isIncremental (Rule _ _ clauses) = all p clauses
  where
    p c = case c of
      Pattern _ _ _ -> True
      RuleClause _ _ -> False

applyRule :: [Fact] -> [DerivedFact] -> Rule -> [DerivedFact]
applyRule facts derived rule = ruleAsFacts facts derived rule

ruleAsFacts :: [Fact] -> [DerivedFact] -> Rule -> [DerivedFact]
ruleAsFacts facts derived (Rule name vars clauses) =
  let allBindings = generateBindings facts derived clauses
  in map (substituteRule name vars) allBindings

substituteRule :: Name -> [Var] -> [(Var, Val)] -> DerivedFact
substituteRule name vars bindings = RuleFact name $ map (unifyVar bindings) vars

unifyVar :: [(Var, Val)] -> Var -> Expr
unifyVar bindings var = maybe (ExprVar var) ExprVal (lookup var bindings)

generateBindings :: [Fact] -> [DerivedFact] -> [Clause] -> [[(Var, Val)]]
generateBindings facts derived [] = []
generateBindings facts derived clauses =
  let goals = map (evalClause facts derived) clauses
  in foldl unifyBindingArrays (head goals) (tail goals)

unifyBindingArrays :: [[(Var, Val)]] -> [[(Var, Val)]] -> [[(Var, Val)]]
unifyBindingArrays arr1 arr2 = concat $
  map (\bindings -> catMaybes $ map (unifyBindings bindings) arr2) arr1

unifyBindings :: [(Var, Val)] -> [(Var, Val)] -> Maybe [(Var, Val)]
unifyBindings bindings1 bindings2 =
  let joined1 = joinMap bindings1 bindings2
      joined2 = joinMap bindings2 bindings1
  in if sort joined1 == sort joined2
    then Just joined1
    else Nothing

joinMap :: Eq a => [(a, b)] -> [(a, b)] -> [(a, b)]
joinMap [] ds = ds
joinMap ((k, v):kvs) ds = (k, v) : joinMap kvs (filter (\(k', _) -> k /= k') ds)

evalClause :: [Fact] -> [DerivedFact] -> Clause -> [[(Var, Val)]]
evalClause facts _ (Pattern ent attr val) =
  let clause = (ent, attr, val)
  in map (unifyFact clause) facts
evalClause _ derived (RuleClause name exprs) =
  let clause = (name, exprs)
      matchedFacts = filter (unifyDerivedFact clause) derived
  in map (asBindingDerivedFact clause) matchedFacts

unifyFact :: (Expr, Attr, Expr) -> Fact -> [(Var, Val)]
unifyFact ((ExprVar x1), attr1, (ExprVar y1)) (Triple ent2 attr2 val2)
  | attr1 == attr2 = [(x1, EntVal ent2), (y1, val2)]
  | otherwise = []
unifyFact ((ExprVal (EntVal ent1)), attr1, (ExprVar y1)) (Triple ent2 attr2 val2)
  | attr1 == attr2 && ent1 == ent2 = [(y1, val2)]
  | otherwise = []
unifyFact ((ExprVar x1), attr1, (ExprVal val1)) (Triple ent2 attr2 val2)
  | attr1 == attr2 && val1 == val2 = [(x1, val2)]
  | otherwise = []
unifyFact _ _ = []

unifyDerivedFact :: (Name, [Expr]) -> DerivedFact -> Bool
unifyDerivedFact (name1, exprs1) (RuleFact name2 exprs2) = and
  [ name1 == name2
  , all (\(k, v) -> k == v || isVariable k || isVariable v) $ zip exprs1 exprs2
  ]

asBindingDerivedFact :: (Name, [Expr]) -> DerivedFact -> [(Var, Val)]
asBindingDerivedFact (name1, exprs1) (RuleFact name2 exprs2) =
  catMaybes $ map f $ zip exprs1 exprs2
  where
    f (ExprVar k, ExprVal v) = Just (k, v)
    f _ = Nothing

isVariable :: Expr -> Bool
isVariable (ExprVar _) = True
isVariable _ = False
