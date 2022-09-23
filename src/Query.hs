module Query where

import Control.Monad
import Data.Maybe
import Data.List

import Data

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

unify :: [Var] -> [WhereClause] -> [Datom] -> Either UniError [[Value]]
unify vars clauses datoms = do
  entities <- (nub . concat) <$> mapM (\w -> unify' w clauses datoms) clauses
  let datoms' = filter (\d -> getId d `elem` entities) datoms
  goEntities entities datoms'
  where
    goEntities [] _ = pure []
    goEntities (e:es) datoms' = do
      let vals = getValues e datoms'
          entVars = catMaybes $ map getEntVarAttr clauses
          valVars = catMaybes $ map getValVarAttr clauses
          f var =
            case (lookup var entVars, lookup var valVars) of
              (Just  attr, _) -> maybe (Left Unknown) Right $ lookup attr vals
              (_, Just attr) -> maybe (Left Unknown) Right $ lookup attr vals
              _ -> Left VarNotUsed
      x <- mapM f vars
      xs <- goEntities es datoms'
      pure $ x : xs
 
getValues :: EntityId -> [Datom] -> [(Attribute, Value)]
getValues id datoms =
  let datoms' = filter (\d -> getId d == id) datoms
  in map (\d -> (getAttr d, getVal d)) datoms'

unify' :: WhereClause -> [WhereClause] -> [Datom] -> Either UniError [EntityId]
unify' clause all datoms = do
  let datoms' = reduce clause datoms
  entities <- mapM (\w -> unifyWith clause w datoms') all
  pure $ minimum entities

reduce :: WhereClause -> [Datom] -> [Datom]
reduce (WhereAnyEntity attr var) datoms =
  filter (\d -> getAttr d == attr) datoms
reduce (WhereAnyValue attr var) datoms =
  filter (\d -> getAttr d == attr) datoms
reduce (WhereValue entVar attr val) datoms =
  filter (\d -> getAttr d == attr && getVal d == val) datoms
reduce (WhereVar entVar attr val) datoms =
  filter (\d -> getAttr d == attr) datoms

unifyOnAttr :: Attribute -> Attribute -> [Datom] -> [EntityId]
unifyOnAttr attr1 attr2 datoms =
  let allIds1 = map getId $ filter (\d -> getAttr d == attr1) datoms
      allIds2 = map getId $ filter (\d -> getAttr d == attr2) datoms
  in filter (`elem` allIds1) allIds2

unifyOnVal :: Attribute -> Attribute -> [Datom] -> Maybe [EntityId]
unifyOnVal attr1 attr2 datoms =
  let allVals1 = map (\d -> (getId d, getVal d)) $ filter (\d -> getAttr d == attr1) datoms
      allIds2 = map getId $ filter (\d -> getAttr d == attr2) datoms
      f (_id, val) = case val of
        EntId id -> Just $ id `elem` allIds2
        _ -> Nothing
  in map (\(id, _val) -> id) <$> filterM f allVals1

unifyWith :: WhereClause -> WhereClause -> [Datom] -> Either UniError [EntityId]
unifyWith w1 w2 datoms =
  case (getEntVarAttr w1, getValVarAttr w1, getEntVarAttr w2, getValVarAttr w2) of
    (Just (entVar1, attr1), _, Just (entVar2, attr2), _) ->
      if entVar1 == entVar2
        then pure $ unifyOnAttr attr1 attr2 datoms
        else pure $ map getId datoms
    (_, Just (valVar1, attr1), Just (entVar2, attr2), _) ->
      if valVar1 == entVar2
        then maybe (Left TypeError) Right $ unifyOnVal attr1 attr2 datoms
        else pure $ map getId datoms
    (_, _, _, _) -> pure $ map getId datoms

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

-- [?artist :artist/name ?artist-name]
-- [?release :release/artist ?artist]
-- [?release :release/name ?release-name]

-- [?artist :artist/name ?artist-name]
-- [_ :release/artist ?artist]

-- [?artist :artist/name ?artist-name]
-- [?release :release/name _]

-- [_ :artist/age ?age]
-- [?person :age ?age]

-- WhereVar "artist" "artist/name" "artist-name"
-- WhereVar "release" "release/artist" "artist"
-- WhereVar "release" "release/name" "release-name"

