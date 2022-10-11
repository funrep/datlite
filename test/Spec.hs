import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Test.Gen

import Query
import Query.Reference.Compile
import Query.Types
import Entity

db :: [Fact]
db =
  [ Triple "alice" ":parent" (EntVal "bob")
  , Triple "alice" ":parent" (EntVal "bill")
  , Triple "bob" ":parent" (EntVal "carol")
  , Triple "carol" ":parent" (EntVal "dennis")
  , Triple "carol" ":parent" (EntVal "david")
  ]

rules :: [Rule]
rules =
  [ Rule "ancestor" ["X", "Y"]
    [ Pattern (ExprVar "X") ":parent" (ExprVar "Y")
    ]
  , Rule "ancestor" ["X", "Y"]
    [ RuleClause "ancestor" [ExprVar "X", ExprVar "Z"]
    , RuleClause "ancestor" [ExprVar "Z", ExprVar "Y"]
    ]
  ]

referenceTests = testGroup "Test reference query engine"
  [ testCase "carol :ancestor Y = dennis, david" $
      toCommonForm (refQueryEngine ["Y"] [RuleClause "ancestor" [ExprVal (EntVal "carol"), ExprVar "Y"]] db rules)
        @?= [[("Y", "dennis")], [("Y", "david")]]
  , testCase "X :ancestor carol = bob, alice" $
      toCommonForm (refQueryEngine ["X"] [RuleClause "ancestor" [ExprVar "X", ExprVal (EntVal "carol")]] db rules)
        @?= [[("X", "bob")], [("X", "alice")]]
  ]

semiNaiveTests = testGroup "Test semi-naive query engine"
  [ testProperty "reference implementation" $ forAll genQueryEngineInput $
      \(vars, clauses, facts, rules) ->
        toCommonForm (q vars clauses facts rules) == toCommonForm (refQueryEngine vars clauses facts rules)
  , testCase "carol :ancestor Y = dennis, david" $
      toCommonForm (q ["Y"] [RuleClause "ancestor" [ExprVal (EntVal "carol"), ExprVar "Y"]] db rules)
        @?= [[("Y", "dennis")], [("Y", "david")]]
  , testCase "X :ancestor carol = bob, alice" $
      toCommonForm (q ["X"] [RuleClause "ancestor" [ExprVar "X", ExprVal (EntVal "carol")]] db rules)
        @?= [[("X", "bob")], [("X", "alice")]]
  ]

main = defaultMain $ testGroup "datlite" 
  [ referenceTests
  , semiNaiveTests
  ]
