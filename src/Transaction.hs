module Transaction where

import Data

data TransactData
  = ASchema Schema
  | AEntity [(Attribute, Value)]
  | Retract EntityId
  deriving (Show, Eq)

newtype Transaction = Transaction [TransactData]

transact :: Transaction -> Db -> Db
transact (Transaction txns) db@(Db txnCount entCount datoms schemas) =
  transact' txns $ Db (txnCount + 1) entCount datoms schemas

transact' :: [TransactData] -> Db -> Db
transact' [] (Db txnCount entCount datoms schemas) =
  Db txnCount entCount datoms schemas
transact' ((ASchema schema):rest) (Db txnCount entCount datoms schemas) =
  let newDb = Db txnCount entCount datoms (schema:schemas)
  in transact' rest newDb
transact' ((AEntity entries):rest) (Db txnCount entCount datoms schemas) =
  let newDb = Db txnCount (entCount + 1) (go entries) schemas
  in transact' rest newDb
  where
    go [] = datoms
    go ((attr, val):rest) = Datom entCount attr val txnCount : go rest
transact' ((Retract entId):rest) (Db txnCount entCount datoms schemas) =
  let datoms' = filter (\d -> getId d == entId || getVal d == EntId entId) datoms
      newDb = Db txnCount entCount datoms' schemas
  in transact' rest newDb
