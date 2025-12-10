module Plutarch.Test.Suite.Plutarch.Numeric (tests) where

import Plutarch.Prelude (PInteger, PNatural, PPositive)
import Plutarch.Test.Methods (
  ppredecessorNBetter,
  pscaleIntegerBetter,
  pscaleNaturalBetter,
  pscalePositiveBetter,
  psuccessorNBetter,
 )
import Plutarch.Unsafe (punsafeCoerce)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Numeric"
    [ testGroup
        "PPositive"
        [ psuccessorNBetter (punsafeCoerce @PPositive @PInteger 20) (punsafeCoerce @PPositive @PInteger 10)
        , pscalePositiveBetter (punsafeCoerce @PPositive @PInteger 20) (punsafeCoerce @PPositive @PInteger 10)
        ]
    , testGroup
        "PNatural"
        [ pscalePositiveBetter (punsafeCoerce @PNatural @PInteger 20) (punsafeCoerce @PPositive @PInteger 10)
        , pscaleNaturalBetter (punsafeCoerce @PNatural @PInteger 20) (punsafeCoerce @PNatural @PInteger 10)
        ]
    , testGroup
        "PInteger"
        [ psuccessorNBetter @PInteger (punsafeCoerce @_ @PInteger 20) 10
        , ppredecessorNBetter @PInteger (punsafeCoerce @_ @PInteger 20) 10
        , pscalePositiveBetter @PInteger 20 (punsafeCoerce @PPositive @PInteger 10)
        , pscaleNaturalBetter @PInteger 20 (punsafeCoerce @PNatural @PInteger 10)
        , pscaleIntegerBetter @PInteger 20 10
        ]
    ]
