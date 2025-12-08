module Plutarch.Test.Suite.PlutarchLedgerApi.V3.Contexts (
  tests,
) where

import Plutarch.LedgerApi.V3.Contexts (
  pfindDatum,
  pfindDatumHash,
  pfindOwnInput,
  pfindTxInByTxOutRef,
  pownCurrencySymbol,
  pspendsOutput,
  ptxSignedBy,
 )
import Plutarch.Prelude (pconstant, plift, (#))
import Plutarch.Test.Utils (prettyEquals)
import PlutusLedgerApi.V3 (
  CurrencySymbol,
  Datum,
  DatumHash,
  PubKeyHash,
  ScriptContext (..),
  ScriptInfo (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut,
  TxOutRef (..),
 )
import PlutusLedgerApi.V3.Contexts (
  findDatum,
  findDatumHash,
  findOwnInput,
  findTxInByTxOutRef,
  ownCurrencySymbol,
  spendsOutput,
  txSignedBy,
 )
import PlutusLedgerApi.V3.Orphans ()
import PlutusTx.AssocMap qualified as AssocMap
import Test.QuickCheck (arbitrary)
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
