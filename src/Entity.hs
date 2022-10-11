module Entity where

type Ent = String

type Attr = String

data Val = StrVal String | EntVal Ent
  deriving (Show, Eq)

data Fact
  = Triple Ent Attr Val
  deriving (Show, Eq)
