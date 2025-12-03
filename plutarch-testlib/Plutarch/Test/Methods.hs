module Plutarch.Test.Methods (
  -- * POrd
  pmaxDefaultBetter,
) where

import Data.ByteString.Short qualified as Short
import Data.Kind (Type)
import Data.SatInt (fromSatInt)
import Data.Tagged (Tagged (Tagged))
import Data.Text qualified as Text
import Plutarch.Evaluate (evalScriptUnlimited)
import Plutarch.Internal.Term (compileOptimized)
import Plutarch.Prelude (
  POrd (pmax, (#<=)),
  S,
  Term,
  pif,
 )
import Plutarch.Script (Script (unScript))
import Plutarch.Test.Utils (typeName)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (ExBudget))
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (ExCPU), ExMemory (ExMemory))
import PlutusLedgerApi.Common (serialiseUPLC)
import Test.Tasty (TestTree)
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  singleTest,
  testFailed,
  testPassed,
 )
import Type.Reflection (Typeable)

{- | Given two arguments to test with, compares the default implementation of
'pmax' to the one defined for the given type. If the defined implementation
is worse than the default in any capacity, the test fails, indicating both
what metric (out of exunits, memory use or script size) was worse, and by how
much; otherwise, the test passes, indicating how much better (if at all) the
defined implementation is compared to the default.

@since wip
-}
pmaxDefaultBetter ::
  forall (a :: S -> Type).
  (POrd a, Typeable a) =>
  (forall (s :: S). Term s a) ->
  (forall (s :: S). Term s a) ->
  TestTree
pmaxDefaultBetter arg1 arg2 = singleTest testName $ PMax arg1 arg2
  where
    testName :: String
    testName = "pmax versus default for " <> typeName @(S -> Type) @a

-- Helpers

data DefaultBetter where
  PMax ::
    forall (a :: S -> Type).
    POrd a =>
    (forall (s :: S). Term s a) ->
    (forall (s :: S). Term s a) ->
    DefaultBetter

instance IsTest DefaultBetter where
  testOptions = Tagged []
  run _ t _ = case t of
    PMax x y -> pure $ case compileOptimized (pif (x #<= y) y x) of
      Left err -> testFailed $ "Failed to compile default implementation of pmax: " <> Text.unpack err
      Right usingDefault -> case compileOptimized (pmax x y) of
        Left err -> testFailed $ "Failed to compile defined implementation of pmax: " <> Text.unpack err
        Right usingImpl -> case evalScriptUnlimited usingDefault of
          (Left err, _, _) -> testFailed $ "Default implementation of pmax did not evaluate: " <> show err
          (Right _, ExBudget (ExCPU cpuDefault) (ExMemory memDefault), _) -> case evalScriptUnlimited usingImpl of
            (Left err, _, _) -> testFailed $ "Defined implementation of pmax did not evaluate: " <> show err
            (Right _, ExBudget (ExCPU cpuImpl) (ExMemory memImpl), _) ->
              let sizeOfDefault = scriptSize usingDefault
                  sizeOfImpl = scriptSize usingImpl
                  exUnitsDefault = fromSatInt @Integer cpuDefault
                  exUnitsImpl = fromSatInt cpuImpl
                  memUnitsDefault = fromSatInt @Integer memDefault
                  memUnitsImpl = fromSatInt memImpl
                  sizeDiff = sizeOfImpl - sizeOfDefault
                  exUnitsDiff = exUnitsImpl - exUnitsDefault
                  memDiff = memUnitsImpl - memUnitsDefault
               in if
                    | sizeDiff < 0 ->
                        testFailed $
                          "Implementation is larger than the default by "
                            <> show (abs sizeDiff)
                            <> " bytes.\nOther diffs (negative means worse implementation):\nExunits: "
                            <> show exUnitsDiff
                            <> "\n"
                            <> "Memory: "
                            <> show memDiff
                            <> "\n"
                    | exUnitsDiff < 0 ->
                        testFailed $
                          "Implementation uses "
                            <> show (abs exUnitsDiff)
                            <> " more exunits than default"
                    | memDiff < 0 ->
                        testFailed $
                          "Implementation uses "
                            <> show (abs memDiff)
                            <> " more memory units than default"
                    | otherwise ->
                        testPassed $
                          "Differences from pmax default:\n"
                            <> "Size: "
                            <> show sizeDiff
                            <> "\n"
                            <> "Exunits: "
                            <> show exUnitsDiff
                            <> "\n"
                            <> "Memory: "
                            <> show memDiff
                            <> "\n"

scriptSize :: Script -> Integer
scriptSize = fromIntegral . Short.length . serialiseUPLC . unScript
