module Plutarch.Test.Suite.PlutarchLedgerApi.V3.Contexts (
  tests,
) where

import Data.Kind (Type)
import Plutarch.LedgerApi.V3.Contexts (
  PScriptContext,
  pfindContinuingOutputs,
  pfindDatum,
  pfindDatumHash,
  pfindOwnInput,
  pfindTxInByTxOutRef,
  pgetContinuingOutputs,
  pownCurrencySymbol,
  ppubKeyOutputsAt,
  pspendsOutput,
  ptxSignedBy,
  pvaluePaidTo,
  pvalueProduced,
  pvalueSpent,
 )
import Plutarch.LedgerApi.Value (pforgetSorted)
import Plutarch.Prelude
import Plutarch.Test.Utils (prettyEquals)
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  CurrencySymbol,
  Datum,
  DatumHash,
  PubKeyHash,
  ScriptContext (..),
  ScriptHash,
  ScriptInfo (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusLedgerApi.V3.Contexts (
  findContinuingOutputs,
  findDatum,
  findDatumHash,
  findOwnInput,
  findTxInByTxOutRef,
  getContinuingOutputs,
  ownCurrencySymbol,
  pubKeyOutputsAt,
  spendsOutput,
  txSignedBy,
  valuePaidTo,
  valueProduced,
  valueSpent,
 )
import PlutusLedgerApi.V3.Orphans ()
import PlutusTx.AssocMap qualified as AssocMap
import Prettyprinter (Pretty)
import Test.QuickCheck (Property, arbitrary, vectorOf)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (forAllShow, testProperty)

tests :: TestTree
tests =
  testGroup
    "Contexts"
    [ testProperty "pfindOwnInput = findOwnInput" $
        forAllShow arbitrary show $ \(ctx :: ScriptContext) ->
          prettyEquals
            (plift $ pfindOwnInput # pconstant ctx)
            (findOwnInput ctx)
    , testProperty "pfindDatum = findDatum" $
        forAllShow arbitrary show $ \(tx :: TxInfo, datum :: Datum, datumHash :: DatumHash, present :: Bool) ->
          let
            tx' =
              tx
                { txInfoData =
                    if present
                      then AssocMap.insert datumHash datum tx.txInfoData
                      else AssocMap.delete datumHash tx.txInfoData
                }
           in
            prettyEquals
              (plift $ pfindDatum # pconstant datumHash # pconstant tx')
              (findDatum datumHash tx')
    , testProperty "pfindDatumHash = findDatumHash" $
        forAllShow arbitrary show $ \(tx :: TxInfo, datum :: Datum, datumHash :: DatumHash, present :: Bool) ->
          let
            tx' =
              tx
                { txInfoData =
                    if present
                      then AssocMap.insert datumHash datum tx.txInfoData
                      else AssocMap.delete datumHash tx.txInfoData
                }
           in
            prettyEquals
              (plift $ pfindDatumHash # pconstant datum # pconstant tx')
              (findDatumHash datum tx')
    , testProperty "pfindTxInByTxOutRef = findTxInByTxOutRef" $
        forAllShow arbitrary show $ \(tx :: TxInfo, inp :: TxInInfo, present :: Bool) ->
          let
            TxInInfo outRef _ = inp
            tx' =
              tx
                { txInfoInputs =
                    if present
                      then tx.txInfoInputs <> [inp]
                      else filter ((/=) outRef . txInInfoOutRef) tx.txInfoInputs
                }
           in
            prettyEquals
              (plift $ pfindTxInByTxOutRef # pconstant outRef # pconstant tx')
              (findTxInByTxOutRef outRef tx')
    , testProperty "pfindContinuingOutputs = findContinuingOutputs" $
        continuingOutputsTest pfindContinuingOutputs findContinuingOutputs
    , testProperty "pgetContinuingOutputs = getContinuingOutputs" $
        continuingOutputsTest pgetContinuingOutputs getContinuingOutputs
    , testProperty "ptxSignedBy = txSignedBy" $
        forAllShow arbitrary show $ \(tx :: TxInfo, pkh :: PubKeyHash, present :: Bool) ->
          let
            tx' =
              tx
                { txInfoSignatories =
                    if present
                      then tx.txInfoSignatories <> [pkh]
                      else filter (/= pkh) tx.txInfoSignatories
                }
           in
            prettyEquals
              (plift $ ptxSignedBy # pconstant tx' # pconstant pkh)
              (txSignedBy tx' pkh)
    , testProperty "ppubKeyOutputsAt = pubKeyOutputsAt" $
        forAllShow arbitrary show $ \(tx :: TxInfo, pkh :: PubKeyHash, seed :: Int) ->
          let
            mkAddr = Address (PubKeyCredential pkh)
            runGen x = unGen x (mkQCGen seed) 30
            flagsWithStakingCreds = runGen $ vectorOf (length tx.txInfoOutputs) arbitrary
            tx' =
              tx
                { txInfoOutputs =
                    zipWith
                      ( \out (isTarget, stakingCred) ->
                          out
                            { txOutAddress =
                                if isTarget then mkAddr stakingCred else out.txOutAddress
                            }
                      )
                      tx.txInfoOutputs
                      flagsWithStakingCreds
                }
           in
            prettyEquals
              ( plift $
                  pmap
                    # plam (pforgetSorted . pto . pfromData)
                    # (ppubKeyOutputsAt # pconstant pkh # pconstant tx')
              )
              (pubKeyOutputsAt pkh tx')
    , testProperty "pvaluePaidTo = valuePaidTo" $
        forAllShow arbitrary show $ \(tx :: TxInfo, pkh :: PubKeyHash, seed :: Int) ->
          let
            mkAddr = Address (PubKeyCredential pkh)
            runGen x = unGen x (mkQCGen seed) 30
            flagsWithStakingCreds = runGen $ vectorOf (length tx.txInfoOutputs) arbitrary
            tx' =
              tx
                { txInfoOutputs =
                    zipWith
                      ( \out (isTarget, stakingCred) ->
                          out
                            { txOutAddress =
                                if isTarget then mkAddr stakingCred else out.txOutAddress
                            }
                      )
                      tx.txInfoOutputs
                      flagsWithStakingCreds
                }
           in
            prettyEquals
              (plift $ pforgetSorted $ pto $ pvaluePaidTo # pconstant tx' # pconstant pkh)
              (valuePaidTo tx' pkh)
    , testProperty "pvalueSpent = valueSpent" $
        forAllShow arbitrary show $ \(tx :: TxInfo) ->
          prettyEquals
            (plift $ pforgetSorted $ pto $ pvalueSpent # pconstant tx)
            (valueSpent tx)
    , testProperty "pvalueProduced = valueProduced" $
        forAllShow arbitrary show $ \(tx :: TxInfo) ->
          prettyEquals
            (plift $ pforgetSorted $ pto $ pvalueProduced # pconstant tx)
            (valueProduced tx)
    , testProperty "pownCurrencySymbol = ownCurrencySymbol (valid script purpose)" $
        forAllShow arbitrary show $ \(ctx :: ScriptContext, cs :: CurrencySymbol) ->
          let ctx' = ctx {scriptContextScriptInfo = MintingScript cs}
           in prettyEquals
                (plift $ pownCurrencySymbol # pconstant ctx')
                (Just $ ownCurrencySymbol ctx')
    , testProperty "pspendsOutput = spendsOutput" $
        forAllShow arbitrary show $ \(tx :: TxInfo, outRef :: TxOutRef, txOut :: TxOut, present :: Bool) ->
          let
            TxOutRef txHash outIdx = outRef
            tx' =
              tx
                { txInfoInputs =
                    if present
                      then tx.txInfoInputs <> [TxInInfo outRef txOut]
                      else filter (\inp -> inp.txInInfoOutRef /= outRef) tx.txInfoInputs
                }
           in
            prettyEquals
              (plift $ pspendsOutput # pconstant tx' # pconstant txHash # pconstant outIdx)
              (spendsOutput tx' txHash outIdx)
    ]

continuingOutputsTest ::
  forall (a :: S -> Type) (b :: Type).
  ( b ~ AsHaskell a
  , PLiftable a
  , Eq b
  , Pretty b
  ) =>
  (forall (s :: S). Term s (PScriptContext :--> a)) ->
  (ScriptContext -> b) ->
  Property
continuingOutputsTest plutarchFn haskellFn =
  forAllShow arbitrary show $
    \(ctx :: ScriptContext, outRef :: TxOutRef, sh :: ScriptHash, txOut :: TxOut, seed :: Int) ->
      let
        scriptAddr = Address (ScriptCredential sh) Nothing
        scriptOut = txOut {txOutAddress = scriptAddr}
        scriptInput = TxInInfo outRef scriptOut
        flags =
          unGen
            (vectorOf (length ctx.scriptContextTxInfo.txInfoOutputs) arbitrary)
            (mkQCGen seed)
            30
        ctx' =
          ctx
            { scriptContextScriptInfo = SpendingScript outRef Nothing
            , scriptContextTxInfo =
                ctx.scriptContextTxInfo
                  { txInfoInputs = ctx.scriptContextTxInfo.txInfoInputs <> [scriptInput]
                  , txInfoOutputs =
                      zipWith
                        ( \out isContinuing ->
                            out
                              { txOutAddress =
                                  if isContinuing then scriptAddr else out.txOutAddress
                              }
                        )
                        ctx.scriptContextTxInfo.txInfoOutputs
                        flags
                  }
            }
       in
        prettyEquals
          (plift $ plutarchFn # pconstant ctx')
          (haskellFn ctx')
