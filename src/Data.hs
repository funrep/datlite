module Data where

import Data.List

type EntityId = Int

type TxnId = Int

type Attribute = String

data Value
  = IntVal Int
  | StrVal String
  | EntId Int
  deriving (Show, Eq)

data Op
  = Add
  | Retract
  deriving (Show, Eq)

data Datom = Datom EntityId Attribute Value TxnId
  deriving (Show, Eq)

getId :: Datom -> EntityId
getId (Datom entId _ _ _) = entId

getAttr :: Datom -> Attribute
getAttr (Datom _ attr _ _) = attr

getVal :: Datom -> Value
getVal (Datom _ _ val _) = val

type Name = String

data Type
  = IntType
  | StrType
  | EntType
  deriving (Show, Eq)

data Cardinality
  = One
  | Many
  deriving (Show, Eq)

data Schema = Schema Name Type
  deriving (Show, Eq)

data Db = Db
  { txnCount :: Int
  , entCount :: Int
  , datoms :: [Datom]
  , schemas :: [Schema]
  }
  deriving (Eq)

instance Show Db where
  show (Db txnCount entCount datoms schemas) =
    show txnCount ++ "\n" ++
    show entCount ++ "\n" ++
    show schemas ++ "\n" ++
    intercalate "\n" (map show datoms)

newDb :: Db
newDb = Db 0 0 [] []
