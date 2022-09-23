module Data where

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
  deriving (Show, Eq)

data Cardinality
  = One
  | Many
  deriving (Show, Eq)

data Schema = Schema Name Type Cardinality
  deriving (Show, Eq)

data Db = Db
  { txnCount :: Int
  , entCount :: Int
  , datoms :: [Datom]
  , schemas :: [Schema]
  }
  deriving (Show, Eq)
