module Transaction where

import Data

data TransactData
  = AddSchema Schema
  | Add TempId Attribute Value
  | RetractEnt EntityId
  | RetractAttr EntityId Attribute
  deriving (Show, Eq)

newtype Transaction = Transaction [TransactData]
  deriving (Show, Eq)

transact :: Transaction -> Db -> Db
transact (Transaction txns) db@(Db txnCount entCount datoms schemas) =
  transact' txns [] $ Db (txnCount + 1) entCount datoms schemas

transact' :: [TransactData] -> [(TempId, EntityId)] -> Db -> Db
transact' [] entIds (Db txnCount entCount datoms schemas) =
  Db txnCount entCount datoms schemas
transact' ((AddSchema schema):rest) entIds (Db txnCount entCount datoms schemas) =
  let newDb = Db txnCount entCount datoms (schema:schemas)
  in transact' rest entIds newDb
transact' ((Add tempId attr value):rest) entIds (Db txnCount entCount datoms schemas) =
  let mEntId = lookup tempId entIds
      value' = case value of
        TempId tempId -> maybe (EntId entCount) EntId $ lookup tempId entIds
        val -> val
      newDatom = Datom (maybe entCount id mEntId) attr value' txnCount
      newDb = Db txnCount (entCount + 1) (newDatom:datoms) schemas
  in transact' rest (maybe entIds (\entId -> (tempId, entId) : entIds) mEntId) newDb
transact' ((RetractEnt entId):rest) entIds (Db txnCount entCount datoms schemas) =
  let datoms' = filter (\d -> getId d == entId || getVal d == EntId entId) datoms
      newDb = Db txnCount entCount datoms' schemas
  in transact' rest entIds newDb
transact' ((RetractAttr entId attr):rest) entIds (Db txnCount entCount datoms schemas) =
  let datoms' = filter (\d -> getId d == entId && getAttr d == attr) datoms
      newDb = Db txnCount entCount datoms' schemas
  in transact' rest entIds newDb
