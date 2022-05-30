{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE TupleSections      #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Contract where

import           Control.Monad             (forever)
import           Control.Lens
import           Data.Text as T
import qualified Data.Map          as Map
import           Prelude as P

import           PlutusTx.Prelude hiding ((.),($),not,(++),(<>),(+),mapM, (<$>),maybe)
import qualified PlutusTx
import           Ledger.Constraints        as Constraints
import           Plutus.Contract           as Contract hiding (tell)
import           Plutus.V1.Ledger.Ada      (lovelaceValueOf)
import           Ledger
import qualified Ledger.Typed.Scripts as Scripts

-- | Validator
data Contracting
instance Scripts.ValidatorTypes Contracting where
    type instance DatumType Contracting = ()
    type instance RedeemerType Contracting = ()

myContractInst :: Scripts.TypedValidator Contracting
myContractInst  = Scripts.mkTypedValidator @Contracting
                  $$(PlutusTx.compile [|| mkValidator ||])
                  $$(PlutusTx.compile [|| Scripts.wrapValidator ||])

myContractValidator :: Validator
myContractValidator = Scripts.validatorScript myContractInst

myContractAddress :: Ledger.Address
myContractAddress = scriptAddress myContractValidator

myRedeemer :: Redeemer
myRedeemer = Redeemer $ PlutusTx.toBuiltinData ()

-- | OffChain logic
type MySchema = Endpoint "consume" ()

endpoints :: Contract () MySchema Text ()
endpoints = forever $ handleError logError $ awaitPromise consumeEp
  where
    consumeEp :: Promise () MySchema Text ()
    consumeEp = endpoint @"consume" $ consumeOp

run :: Contract () MySchema Text ()
run = start >> endpoints

start :: Contract () MySchema Text ()
start = do
    logInfo @String "Starting contract..."
    currTime <- currentTime

    let tx  =  Constraints.mustPayToTheScript () (lovelaceValueOf 2_000_000)
        lkp =  Constraints.typedValidatorLookups myContractInst
            <> Constraints.otherScript myContractValidator

    _ <- submitTxConstraintsWith @Contracting lkp tx
    logInfo @String "Contract started"

consumeOp :: () -> Contract () MySchema Text ()
consumeOp _ = do
    (oref,outxo) <- getUtxo
    currTime <- currentTime

    let tx  =  Constraints.mustSpendScriptOutput oref myRedeemer
            <> Constraints.mustValidateIn
                   (interval currTime $ currTime + windowSize)

        lkp =  Constraints.unspentOutputs (Map.fromList [(oref,outxo)])
            <> Constraints.typedValidatorLookups myContractInst
            <> Constraints.otherScript myContractValidator

    _  <- submitTxConstraintsWith @Contracting lkp tx
    logInfo @String "Utxo consumed succesfully"
  where
    windowSize :: POSIXTime
    windowSize = fromMilliSeconds (DiffMilliSeconds 600_000)

    getUtxo :: Contract () MySchema Text (TxOutRef, ChainIndexTxOut)
    getUtxo = do
      utxos <- (mapM (\ (oref, o) -> ciTxOutDatum loadDatum o <&> (oref,)) . Map.toList) =<< utxosAt myContractAddress
      case utxos of
          [] -> throwError "No UTxOs available."
          l  -> return (P.head l)

    loadDatum :: Either DatumHash Datum -> Contract w s T.Text (Either DatumHash Datum)
    loadDatum ldh@(Left dh) = maybe ldh Right <$> datumFromHash dh
    loadDatum d = return d

-- | OnChain logic
{-# INLINABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ ctx = traceIfFalse "Empty validity range" $ (not . isEmpty) txInterval
  where
    txInterval :: POSIXTimeRange
    txInterval = txInfoValidRange $ scriptContextTxInfo ctx

