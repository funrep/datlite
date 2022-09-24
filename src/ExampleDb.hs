module ExampleDb where

import Data
import Transaction

movies :: Db
movies =
  let db0 = newDb
      db1 = transact (Transaction
        [ ASchema $ Schema ":artist/name" StrType
        , ASchema $ Schema ":album/title" StrType
        , ASchema $ Schema ":album/artist" EntType
        , ASchema $ Schema ":album/length" IntType
        ]) db0
      db2 = transact (Transaction
        [ AEntity [(":artist/name", StrVal "Mr. Pizza")]
        , AEntity [(":artist/name", StrVal "Badass")]
        , AEntity [(":album/title", StrVal "Mama"), (":album/length", IntVal 32), (":album/artist", EntId 0)]
        , AEntity [(":album/title", StrVal "Yo la"), (":album/length", IntVal 15), (":album/artist", EntId 0)]
        ]) db1
  in db2
