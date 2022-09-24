module Query where

import Control.Monad
import Data.Maybe
import Data.List

import Data

import Debug.Trace (trace, traceM)

type Var = String

data Query
  = Find [Var] [WhereClause]
  deriving (Show, Eq)

data WhereClause
  = WhereAnyEntity Attribute Var
  | WhereAnyValue Var Attribute
  | WhereValue Var Attribute Value
  | WhereVar Var Attribute Var
  deriving (Show, Eq)

data UniError
  = TypeError
  | VarNotUsed
  | Unknown
  deriving (Show, Eq)

query :: Query -> Db -> Either UniError [[Value]]
query (Find vars clauses) (Db _ _ datoms _) = unify vars clauses datoms

-- 1. for every clause
-- 2. filter entities by
--    1. attr and val
--    3. solve variable constraints
-- 3. go through entities, each entity that has the information we return

unify :: [Var] -> [WhereClause] -> [Datom] -> Either UniError [[Value]]
unify vars clauses datoms = do
  let filteredClauses = map (\w -> (w, filterClause w datoms)) clauses
  entities <- mapM (\w -> unify' w filteredClauses datoms) filteredClauses
  let entities' = nub $ concat entities
      datoms' = filter (\d -> getId d `elem` entities') datoms
  traceM $ show filteredClauses
  pure $ goEntities entities' datoms'
  where
    goEntities [] _ = []
    goEntities (e:es) datoms' = do
      let vals = getValues e datoms'
          entVars = catMaybes $ map getEntVarAttr clauses
          valVars = catMaybes $ map getValVarAttr clauses
          f var =
            case (lookup var entVars, lookup var valVars) of
              (Just _attr, _) -> Just $ EntId e
              (_, Just attr) -> lookup attr vals
              _ -> Nothing
          row = catMaybes $ map f vars
          rest = goEntities es datoms'
      if length row == length vars
        then row : rest
        else rest

getValues :: EntityId -> [Datom] -> [(Attribute, Value)]
getValues id datoms =
  let datoms' = filter (\d -> getId d == id) datoms
  in map (\d -> (getAttr d, getVal d)) datoms'

unify' :: (WhereClause, [EntityId]) -> [(WhereClause, [EntityId])] -> [Datom] -> Either UniError [EntityId]
unify' clause all datoms = do
  entities <- mapM (\w -> unifyWith clause w datoms) all
  pure $ minimum entities

filterClause :: WhereClause -> [Datom] -> [EntityId]
filterClause (WhereAnyEntity attr _) datoms = map getId $ filter (\d -> getAttr d == attr) datoms
filterClause (WhereAnyValue _ attr) datoms = map getId $ filter (\d -> getAttr d == attr) datoms
filterClause (WhereValue _ attr val) datoms = map getId $ filter (\d -> getAttr d == attr && getVal d == val) datoms
filterClause (WhereVar _ attr _) datoms = map getId $ filter (\d -> getAttr d == attr) datoms

unifyWith :: (WhereClause, [EntityId]) -> (WhereClause, [EntityId]) -> [Datom] -> Either UniError [EntityId]
unifyWith (w1, ents1) (w2, ents2) datoms
  | w1 == w2 = pure $ ents1
  | otherwise = 
    case (getEntVarAttr w1, getValVarAttr w1, getEntVarAttr w2, getValVarAttr w2) of
      (Just (entVar1, attr1), _, Just (entVar2, attr2), _) ->
        if entVar1 == entVar2
          then pure $ filter (`elem` ents1) ents2
          else pure $ ents1
      (_, Just (valVar1, attr1), Just (entVar2, attr2), _) ->
        if valVar1 == entVar2
          then maybe (Left TypeError) Right $ unifyOnValVar2 (attr1, ents1) ents2 datoms
          else pure $ ents1
      (_, _, _, _) -> pure $ ents1

unifyOnValVar2 :: (Attribute, [EntityId]) -> [EntityId] -> [Datom] -> Maybe [EntityId]
unifyOnValVar2 (attr1, ents1) ents2 datoms =
  let vals1 = map (\d -> (getId d, getVal d)) $ filter (\d -> getId d `elem` ents1 && getAttr d == attr1) datoms
      f (_id, val) = case val of
        EntId id -> Just $ id `elem` ents2
        _ -> Nothing
  in map (\(id, _val) -> id) <$> filterM f vals1

getEntVarAttr :: WhereClause -> Maybe (Var, Attribute)
getEntVarAttr (WhereVar entVar attr _) = Just (entVar, attr)
getEntVarAttr (WhereValue entVar attr _) = Just (entVar, attr)
getEntVarAttr (WhereAnyValue entVar attr) = Just (entVar, attr)
getEntVarAttr (WhereAnyEntity _ _) = Nothing

getValVarAttr :: WhereClause -> Maybe (Var, Attribute)
getValVarAttr (WhereVar _ attr valVar) = Just (valVar, attr)
getValVarAttr (WhereAnyEntity attr valVar) = Just (valVar, attr)
getValVarAttr (WhereValue _ _ _) = Nothing
getValVarAttr (WhereAnyValue _ _) = Nothing
