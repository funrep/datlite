module Transaction where

import Data

data TransactData
  = ASchema Schema
  | AData Attribute Value
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
transact' ((AData attr val):rest) (Db txnCount entCount datoms schemas) =
  let entCount' = entCount + 1
      datom = Datom entCount' attr val txnCount
      newDb = Db txnCount entCount' (datom:datoms) schemas
  in transact' rest newDb
