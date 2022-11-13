module Query.SemiNaive where

import Data.Maybe
import Data.List
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Entity
import Query.Types

semiNaiveQ :: QueryEngine
semiNaiveQ vars clauses facts rules =
  map (M.assocs . findAll vars) $ generateBindings facts (derive facts rules) clauses

findAll :: [Var] -> Map Var Val -> Map Var Val
findAll ks m = foldl (\res k -> maybe res (\v -> M.insert k v res) $ M.lookup k m) mempty ks

derive :: [Fact] -> [Rule] -> [DerivedFact]
derive facts rules =
  let derived = foldl (applyRule facts) mempty $ incrementalRules rules
  in deriveRec facts derived derived $ restOfRules rules

deriveRec :: [Fact] -> [DerivedFact] -> [DerivedFact] -> [Rule] -> [DerivedFact] 
deriveRec facts derived allDerived rules =
  let newDerived = S.fromList $ foldl (applyRule facts) derived rules
      diff = S.toList $ newDerived S.\\ (S.fromList derived)
      join = S.toList $ (S.fromList allDerived) `S.union` newDerived
  in if null newDerived
    then allDerived
    else deriveRec facts diff join rules

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

substituteRule :: Name -> [Var] -> Map Var Val -> DerivedFact
substituteRule name vars bindings = RuleFact name $ map (unifyVar bindings) vars

unifyVar :: Map Var Val -> Var -> Expr
unifyVar bindings var = maybe (ExprVar var) ExprVal (M.lookup var bindings)

generateBindings :: [Fact] -> [DerivedFact] -> [Clause] -> [Map Var Val]
generateBindings facts derived [] = []
generateBindings facts derived clauses =
  let goals = map (evalClause facts derived) clauses
  in foldl unifyBindingArrays (head goals) (tail goals)

unifyBindingArrays :: [Map Var Val] -> [Map Var Val] -> [Map Var Val]
unifyBindingArrays arr1 arr2 = concat $
  map (\bindings -> catMaybes $ map (unifyBindings bindings) arr2) arr1

unifyBindings :: Map Var Val -> Map Var Val -> Maybe (Map Var Val)
unifyBindings bindings1 bindings2 =
  let joined1 = M.union bindings1 bindings2
      joined2 = M.union bindings2 bindings1
  in if joined1 == joined2
    then Just joined1
    else Nothing

evalClause :: [Fact] -> [DerivedFact] -> Clause -> [Map Var Val]
evalClause facts _ (Pattern ent attr val) =
  let clause = (ent, attr, val)
  in filter (\m -> m /= mempty) $ map (unifyFact clause) facts
evalClause _ derived (RuleClause name exprs) =
  let clause = (name, exprs)
      matchedFacts = filter (unifyDerivedFact clause) derived
  in map (asBindingDerivedFact clause) matchedFacts

unifyFact :: (Expr, Attr, Expr) -> Fact -> Map Var Val
unifyFact ((ExprVar x1), attr1, (ExprVar y1)) (Triple ent2 attr2 val2)
  | attr1 == attr2 = M.fromList $ [(x1, EntVal ent2), (y1, val2)]
  | otherwise = mempty
unifyFact ((ExprVal (EntVal ent1)), attr1, (ExprVar y1)) (Triple ent2 attr2 val2)
  | attr1 == attr2 && ent1 == ent2 = M.fromList $ [(y1, val2)]
  | otherwise = mempty
unifyFact ((ExprVar x1), attr1, (ExprVal val1)) (Triple ent2 attr2 val2)
  | attr1 == attr2 && val1 == val2 = M.fromList $ [(x1, val2)]
  | otherwise = mempty
unifyFact _ _ = mempty

unifyDerivedFact :: (Name, [Expr]) -> DerivedFact -> Bool
unifyDerivedFact (name1, exprs1) (RuleFact name2 exprs2) = and
  [ name1 == name2
  , all (\(k, v) -> k == v || isVariable k || isVariable v) $ zip exprs1 exprs2
  ]

asBindingDerivedFact :: (Name, [Expr]) -> DerivedFact -> Map Var Val
asBindingDerivedFact (name1, exprs1) (RuleFact name2 exprs2) =
  M.fromList $ catMaybes $ map f $ zip exprs1 exprs2
  where
    f (ExprVar k, ExprVal v) = Just (k, v)
    f _ = Nothing

isVariable :: Expr -> Bool
isVariable (ExprVar _) = True
isVariable _ = False
