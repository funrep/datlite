module ExampleDb where

import Data
import Transaction

movies :: Db
movies =
  let db0 = newDb
      db1 = transact (Transaction
        [ AddSchema $ Schema ":artist/name" StrType
        , AddSchema $ Schema ":album/title" StrType
        , AddSchema $ Schema ":album/artist" EntType
        , AddSchema $ Schema ":album/length" IntType
        ]) db0
      db2 = transact (Transaction
        [ Add "pizza" ":artist/name" (StrVal "Mr. Pizza")
        , Add "badass" ":artist/name" (StrVal "Badass")
        , Add "mama" ":album/title" (StrVal "Mama")
        , Add "mama" ":album/length" (IntVal 32)
        , Add "mama" ":album/artist" (TempId "pizza")
        , Add "yo" ":album/title" (StrVal "Yo la")
        , Add "yo" ":album/length" (IntVal 15)
        , Add "yo" ":album/artist" (TempId "pizza")
        ]) db1
  in db2
