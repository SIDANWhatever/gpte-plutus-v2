{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GPTE.TreasuryValidator (validator, customValidator) where

-- are all of these necessary?

import GPTE.Types
import Ledger (scriptHashAddress)
import qualified Ledger.Ada as Ada
import Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator, ValidatorTypes, mkTypedValidator, mkTypedValidatorParam, mkUntypedValidator, validatorScript)
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Show (..))
import qualified Prelude as Pr

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import SIDANDefaultOrphans()

{-# INLINEABLE treasuryValidator #-}
treasuryValidator :: TreasuryParam -> TreasuryDatum -> TreasuryAction -> ScriptContext -> Bool
treasuryValidator tp dat action ctx =
  case action of
    (Commit b) ->
      traceIfFalse "Contrib token missing from input" inputHasContribToken
        && traceIfFalse "Contrib token missing from contract output" contractOutputHasContribToken
        && traceIfFalse "Output Value must match ProjectDetails" (checkValueToEscrowContract b)
        && traceIfFalse "Treasury must keep remaining lovelace" (treasuryGetsLovelaceBack b)
        && traceIfFalse "Treasury must keep remaining tokens" (treasuryGetsTokensBack b)
        && traceIfFalse "Not a valid Project hash" (checkProjectHash b)
        && traceIfFalse "In and out datum must match" checkDatum
        && traceIfFalse "Redeemer and escrow datum not the same" (checkProjectDetailsIsEscrowDatum b)
    Manage -> traceIfFalse "Only Issuer can change Treasury" inputHasIssuerToken
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Create a list of all CurrencySymbol in tx input
    inVals :: [CurrencySymbol]
    inVals = symbols $ valueSpent info

    -- case action -> Manage:
    -- If the TreasuryAction is "Manage", then input must have an IssuerToken
    inputHasIssuerToken :: Bool
    inputHasIssuerToken = tIssuerPolicyId tp `elem` inVals

    -- case action -> Commit:
    -- Check that list of CurrencySymbols includes Contrib CurrencySymbol
    inputHasContribToken :: Bool
    inputHasContribToken = tContribTokenPolicyId tp `elem` inVals

    -- The Value to be included in Escrow Contract UTXO
    toEscrowContract :: Value
    toEscrowContract = valueLockedBy info (escrowContractHash tp)

    -- Check that the Contrib Token is sent to Escrow Contract UTXO
    contractOutputHasContribToken :: Bool
    contractOutputHasContribToken = tContribTokenPolicyId tp `elem` symbols toEscrowContract

    -- Check that the Value sent to Contract UTXO matches what is specified in the Redeemer
    checkValueToEscrowContract :: ProjectDetails -> Bool
    checkValueToEscrowContract b =
      Ada.getLovelace (Ada.fromValue toEscrowContract) == lovelaceAmount b
        && valueOf toEscrowContract (tProjectTokenPolicyId tp) (tProjectTokenName tp) == tokenAmount b

    -- The UTXO input from Treasury
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "treasury input missing"
      Just i -> txInInfoResolved i

    -- The UTXO output back to Treasury
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o -- There must be exactly ONE output UTXO
      _ -> traceError "expected exactly one treasury output"

    -- Values of each
    treasuryInputValue :: Value
    treasuryInputValue = txOutValue ownInput

    treasuryOutputValue :: Value
    treasuryOutputValue = txOutValue ownOutput

    -- Compare Values from and to Treasury to make sure that Treasury gets the right value back
    treasuryGetsLovelaceBack :: ProjectDetails -> Bool
    treasuryGetsLovelaceBack b = lovelaceToTreasury == lovelaceFromTreasury - lovelaceToCommitment
      where
        lovelaceFromTreasury = Ada.getLovelace (Ada.fromValue treasuryInputValue)
        lovelaceToTreasury = Ada.getLovelace (Ada.fromValue treasuryOutputValue)
        lovelaceToCommitment = lovelaceAmount b

    treasuryGetsTokensBack :: ProjectDetails -> Bool
    treasuryGetsTokensBack b = gimbalsToTreasury == gimbalsFromTreasury - gimbalsToCommitment
      where
        gimbalsFromTreasury = valueOf treasuryInputValue (tProjectTokenPolicyId tp) (tProjectTokenName tp)
        gimbalsToTreasury = valueOf treasuryOutputValue (tProjectTokenPolicyId tp) (tProjectTokenName tp)
        gimbalsToCommitment = tokenAmount b

    -- Check that the project hash in redeemer matches one of the hashes in treasury datum.
    checkProjectHash :: ProjectDetails -> Bool
    checkProjectHash b = (projectHash b) `elem` (projectHashList dat)

    checkDatum :: Bool
    checkDatum = txOutDatum ownInput == txOutDatum ownOutput

    --get utxo to escrow contract, in case only one
    escrowOutput :: TxOut
    escrowOutput =
      let ins =
            [ i
              | i <- txInfoOutputs info,
                txOutAddress i == scriptHashAddress (escrowContractHash tp)
            ]
       in case ins of
            [o] -> o
            _ -> traceError "expected exactly one escrow output"

    -- escrow datum should be inline and same as project details
    checkProjectDetailsIsEscrowDatum :: ProjectDetails -> Bool
    checkProjectDetailsIsEscrowDatum bd = case txOutDatum escrowOutput of
      OutputDatum d -> d == Datum (toBuiltinData bd)
      _ -> traceError "wrong datum type"

typedValidator :: TreasuryParam -> TypedValidator TreasuryTypes
typedValidator tp = go tp
  where
    go =
      mkTypedValidatorParam @TreasuryTypes
        $$(PlutusTx.compile [||treasuryValidator||])
        $$(PlutusTx.compile [||wrap||])
    wrap = mkUntypedValidator

validator :: TreasuryParam -> Validator
validator = validatorScript . typedValidator

data MyTreasuryParam = MyTreasuryParam {
    cTContribTokenPolicyId :: CurrencySymbol
  , cEscrowContractHash :: ValidatorHash 
  , cTProjectTokenPolicyId :: CurrencySymbol
  , cTProjectTokenName :: TokenName
} deriving (Generic, FromJSON, ToJSON)

customValidator :: MyTreasuryParam -> Validator
customValidator (MyTreasuryParam a b c d) = validator $ TreasuryParam { 
    tIssuerPolicyId = CurrencySymbol "2b0a04a7b60132b1805b296c7fcb3b217ff14413991bf76f72663c30"
  , tContribTokenPolicyId = a
  , escrowContractHash = b
  , tProjectTokenPolicyId = c
  , tProjectTokenName = d
}
