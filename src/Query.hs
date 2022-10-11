module Query where

import Entity
import Query.Types
import Query.SemiNaive

q :: [Var] -> [Clause] -> [Fact] -> [Rule] -> [[(Var, Val)]]
q = semiNaiveQ
