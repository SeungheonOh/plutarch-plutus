module Plutarch.Test.Suite.Plutarch.Numeric (tests) where

import Plutarch.Prelude (PInteger, PPositive)
import Plutarch.Test.Methods (psuccessorNBetter)
import Plutarch.Unsafe (punsafeCoerce)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Numeric"
    [ testGroup
        "PPositive"
        [ psuccessorNBetter (punsafeCoerce @PPositive @PInteger 20) (punsafeCoerce @PPositive @PInteger 10)
        ]
    , testGroup
        "PInteger"
        [ psuccessorNBetter @PInteger (punsafeCoerce @_ @PInteger 20) 10
        ]
    ]
