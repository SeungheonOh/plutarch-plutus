{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Test.Suite.Plutarch.Array (tests) where

import Plutarch.Array (
  PPullArray,
  pdropArray,
  pfoldArray,
  pfromArray,
  pfromList,
  pgenerate,
  pimapArray,
  piota,
  pizipWithArray,
  pmapArray,
  ppullArrayToList,
  ppullArrayToSOPList,
  prfoldArray,
  ptakeArray,
  pzipWithArray,
 )
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, plutarchGolden)
import Plutarch.Unsafe (punsafeCoerce)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "PPullArray"
    [ plutarchGolden
        "Goldens"
        "array"
        [ goldenEval "piota" sample
        , goldenEval "pgenerate" sample2
        , goldenEval "pfromArray" (pfromArray . pconstant @(PArray PInteger) $ [1, 2 .. 10])
        , goldenEval "pfromList" (pfromList . pconstant @(PBuiltinList PInteger) $ [1, 2 .. 10])
        , goldenEval "pmapArray" (pmapArray plusTen sample)
        , goldenEval "pimapArray" (pimapArray (plam pscaleNatural) sample)
        , goldenEval "ptakeArray" (ptakeArray (unsafeNat 4) sample)
        , goldenEval "pdropArray" (pdropArray (unsafeNat 4) sample)
        , goldenEval "pzipWithArray (different)" (pzipWithArray (plam $ flip pscaleNatural) sample sample2)
        , goldenEval "pzipWithArray (same, no plet)" (pzipWithArray (plam (#*)) sample sample)
        , goldenEval "pzipWithArray (same, plet)" (plet sample $ \sample' -> pzipWithArray (plam (#*)) sample' sample')
        , goldenEval "pizipWithArray" (pizipWithArray (plam $ \i x y -> pscaleNatural (y #+ i) x) sample sample2)
        , goldenEval "pfoldArray" (pfoldArray (plam $ \acc x -> acc #- x) 0 sample2)
        , goldenEval "prfoldArray" (prfoldArray (plam $ \acc x -> acc #- x) 0 sample2)
        , goldenEval "ppullArrayToList" (ppullArrayToList sample)
        , goldenEval "ppullArrayToSOPList" (ppullArrayToSOPList sample)
        ]
    , plutarchGolden
        "Goldens"
        "array-fusion"
        [ goldenEval "pmap then pimap" (pimapArray (plam pscaleNatural) . pmapArray plusTen $ sample)
        , goldenEval "pmap then ptake" (ptakeArray (unsafeNat 4) . pmapArray plusTen $ sample)
        , goldenEval "pmap then pzipWith" (pzipWithArray (plam $ flip pscaleNatural) (pmapArray plusTen sample) (pmapArray (plam (+ 10)) sample2))
        , goldenEval "pmap then pfold" (pfoldArray (plam $ \acc x -> acc #- x) 0 . pmapArray (plam (+ 10)) $ sample2)
        ]
    ]

-- Helpers

len :: forall (s :: S). Term s PNatural
len = unsafeNat 10

unsafeNat :: forall (s :: S). Term s PInteger -> Term s PNatural
unsafeNat = punsafeCoerce @PNatural @PInteger

sample :: forall (s :: S). Term s (PPullArray PNatural)
sample = piota len

sample2 :: forall (s :: S). Term s (PPullArray PInteger)
sample2 = pgenerate len (plam $ \x -> x #* 2)

plusTen :: forall (s :: S). Term s (PNatural :--> PNatural)
plusTen = plam $ \x -> x #+ len
