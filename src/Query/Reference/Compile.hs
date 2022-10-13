module Query.Reference.Compile where

import qualified Query.Reference.Naive as Ref
import Query.SemiNaive
import Query.Types
import Entity

refQueryEngine :: QueryEngine
refQueryEngine vars clauses facts rules =
  let res = Ref.referenceQ vars (map toRefClause clauses) (map toRefFact facts) (map toRefRule rules)
  in map (map (\(k, v) -> (k, toVal v))) res
  where
    toRefFact (Triple ent attr val) = Ref.Triple ent attr (toRefVal val)

    toRefRule (Rule name vars clauses) = Ref.Rule name vars (map toRefClause clauses)

    toRefClause (Pattern expr1 attr expr2) = Ref.Pattern (toRefExpr expr1) attr (toRefExpr expr2)
    toRefClause (RuleClause name exprs) = Ref.RuleClause name (map toRefExpr exprs)

    toRefExpr (ExprVal val) = toRefVal val
    toRefExpr (ExprVar var) = var

    toRefVal (StrVal val) = val
    toRefVal (EntVal ent) = ent

    toVal val = EntVal val

toCommonForm :: [[(Var, Val)]] -> [[(String, String)]]
toCommonForm = map (map (\(k, v) -> (k, fromVal v)))
  where
    fromVal (StrVal val) = val
    fromVal (EntVal ent) = ent
