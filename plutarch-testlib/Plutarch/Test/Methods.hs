{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Test.Methods (
  -- * POrd
  pmaxDefaultBetter,
  pminDefaultBetter,
) where

import Data.Bifunctor (first)
import Data.ByteString.Short qualified as Short
import Data.Kind (Type)
import Data.SatInt (fromSatInt)
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.Text qualified as Text
import Plutarch.Evaluate (evalScriptUnlimited)
import Plutarch.Internal.Term (compileOptimized)
import Plutarch.Prelude (
  POrd (pmax, pmin, (#<=)),
  S,
  Term,
  pif,
  plam,
  (#),
  (:-->),
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

{- | Given two arguments to test with, compares the default implementation of
'pmin' to the one defined for the given type. If the defined implementation
is worse than the default in any capacity, the test fails, indicating both
what metric (out of exunits, memory use or script size) was worse, and by how
much; otherwise, the test passes, indicating how much better (if at all) the
defined implementation is compared to the default.

@since wip
-}
pminDefaultBetter ::
  forall (a :: S -> Type).
  (POrd a, Typeable a) =>
  (forall (s :: S). Term s a) ->
  (forall (s :: S). Term s a) ->
  TestTree
pminDefaultBetter arg1 arg2 = singleTest testName $ PMin arg1 arg2
  where
    testName :: String
    testName = "pmin versus default for " <> typeName @(S -> Type) @a

-- Helpers

data DefaultBetter where
  PMax ::
    forall (a :: S -> Type).
    POrd a =>
    (forall (s :: S). Term s a) ->
    (forall (s :: S). Term s a) ->
    DefaultBetter
  PMin ::
    forall (a :: S -> Type).
    POrd a =>
    (forall (s :: S). Term s a) ->
    (forall (s :: S). Term s a) ->
    DefaultBetter

instance IsTest DefaultBetter where
  testOptions = Tagged []
  run _ t _ = pure $ case t of
    PMax x y -> case tryAndApply
      "pmax"
      (plam $ \x' y' -> pif (x' #<= y') y x)
      (plam $ \x' y' -> pmax x' y')
      x
      y of
      Left failText -> testFailed failText
      Right deltas@(sizeDelta, exUnitDelta, memDelta) ->
        if (sizeDelta >= 0) && (exUnitDelta >= 0) && (memDelta >= 0)
          then testPassed . formatDelta $ deltas
          else testFailed . formatDelta $ deltas
    PMin x y -> case tryAndApply
      "pmin"
      (plam $ \x' y' -> pif (x' #<= y') x' y')
      (plam $ \x' y' -> pmin x' y')
      x
      y of
      Left failText -> testFailed failText
      Right deltas@(sizeDelta, exUnitDelta, memDelta) ->
        if (sizeDelta >= 0) && (exUnitDelta >= 0) && (memDelta >= 0)
          then testPassed . formatDelta $ deltas
          else testFailed . formatDelta $ deltas

scriptSize :: Script -> Integer
scriptSize = fromIntegral . Short.length . serialiseUPLC . unScript

tryAndApply ::
  forall (a :: S -> Type).
  String ->
  (forall (s :: S). Term s (a :--> a :--> a)) ->
  (forall (s :: S). Term s (a :--> a :--> a)) ->
  (forall (s :: S). Term s a) ->
  (forall (s :: S). Term s a) ->
  Either String (Integer, Integer, Integer)
tryAndApply op defaultImpl definedImpl x y = do
  usingDefault <- first (formatCompileErr "default") . compileOptimized $ defaultImpl # x # y
  usingImpl <- first (formatCompileErr "defined") . compileOptimized $ definedImpl # x # y
  (cpuDefault, memDefault) <- tryEval "Default" usingDefault
  (cpuImpl, memImpl) <- tryEval "Defined" usingImpl
  let sizeDelta = scriptSize usingImpl - scriptSize usingDefault
  let exunitDelta = cpuImpl - cpuDefault
  let memDelta = memImpl - memDefault
  pure (sizeDelta, exunitDelta, memDelta)
  where
    formatCompileErr :: String -> Text -> String
    formatCompileErr which err =
      "Failed to compile "
        <> which
        <> "implementation of "
        <> op
        <> ": "
        <> Text.unpack err
    tryEval :: String -> Script -> Either String (Integer, Integer)
    tryEval which s = case evalScriptUnlimited s of
      (Left err, _, _) -> Left $ which <> " implementation of " <> op <> " did not evaluate: " <> show err
      (Right _, ExBudget (ExCPU cpu) (ExMemory mem), _) -> pure (fromSatInt @Integer cpu, fromSatInt @Integer mem)

formatDelta :: (Integer, Integer, Integer) -> String
formatDelta (sizeDelta, exUnitDelta, memDelta) =
  "Difference from default implementation (negative means worse):\n"
    <> "Script size: "
    <> show sizeDelta
    <> "\n"
    <> "Exunits: "
    <> show exUnitDelta
    <> "\n"
    <> "Memory: "
    <> show memDelta
    <> "\n"
