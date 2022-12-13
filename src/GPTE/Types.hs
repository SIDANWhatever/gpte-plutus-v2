{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GPTE.Types
  ( TreasuryParam (..),
    TreasuryDatum (..),
    ProjectDetails (..),
    TreasuryAction (..),
    ProjectAction (..),
    EscrowParam (..),
    CommitmentEscrowDatum (..),
    TreasuryTypes,
    EscrowTypes,
  )
where

import GHC.Generics (Generic)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator, ValidatorTypes, mkTypedValidator, mkTypedValidatorParam, mkUntypedValidator, validatorScript)
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Show (..))
import qualified Prelude as Pr

import Data.Aeson (FromJSON, ToJSON)
import SIDANDefaultOrphans()

data TreasuryParam = TreasuryParam
  { tIssuerPolicyId :: CurrencySymbol,
    tContribTokenPolicyId :: CurrencySymbol,
    escrowContractHash :: ValidatorHash,
    tProjectTokenPolicyId :: CurrencySymbol,
    tProjectTokenName :: TokenName
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''TreasuryParam

-- consider representing Issuer with a token, instead of PKH
data TreasuryDatum = TreasuryDatum
  { projectHashList :: [BuiltinByteString],
    issuerTokenName :: TokenName
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.unstableMakeIsData ''TreasuryDatum

data ProjectDetails = ProjectDetails
  { contributorPkh :: PubKeyHash,
    lovelaceAmount :: Integer,
    tokenAmount :: Integer,
    expirationTime :: POSIXTime,
    projectHash :: BuiltinByteString
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic)

instance Eq ProjectDetails where
  {-# INLINEABLE (==) #-}
  ProjectDetails cP lA tA eT bH == ProjectDetails cP' lA' tA' eT' bH' =
    (cP == cP') && (lA == lA') && (tA == tA') && (eT == eT') && (bH == bH')

PlutusTx.unstableMakeIsData ''ProjectDetails
PlutusTx.makeLift ''ProjectDetails

data TreasuryAction = Commit ProjectDetails | Manage
  deriving (Show)

PlutusTx.makeIsDataIndexed ''TreasuryAction [('Commit, 0), ('Manage, 1)]
PlutusTx.makeLift ''TreasuryAction

data TreasuryTypes

instance ValidatorTypes TreasuryTypes where
  type DatumType TreasuryTypes = TreasuryDatum
  type RedeemerType TreasuryTypes = TreasuryAction

data CommitmentEscrowDatum = CommitmentEscrowDatum
  { bedContributorPkh :: PubKeyHash,
    bedLovelaceAmount :: Integer,
    bedTokenAmount :: Integer,
    bedExpirationTime :: POSIXTime,
    bedProjectHash :: BuiltinByteString
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic)

instance Eq CommitmentEscrowDatum where
  {-# INLINEABLE (==) #-}
  CommitmentEscrowDatum bCP bLA bTA bET bBH == CommitmentEscrowDatum bCP' bLA' bTA' bET' bBH' =
    (bCP == bCP') && (bLA == bLA') && (bTA == bTA') && (bET == bET') && (bBH == bBH')

-- Alternative way of comparisons
-- a == b = (bedIssuerPkh       a == bedIssuerPkh      b) &&
--          (bedContributorPkh  a == bedContributorPkh b) &&
--          (bedLovelaceAmount  a == bedLovelaceAmount b) &&
--          (bedTokenAmount     a == bedTokenAmount    b) &&
--          (bedExpirationTime  a == bedExpirationTime b)

PlutusTx.unstableMakeIsData ''CommitmentEscrowDatum
PlutusTx.makeLift ''CommitmentEscrowDatum

data EscrowParam = EscrowParam
  { projectTokenPolicyId :: CurrencySymbol,
    projectTokenName :: TokenName,
    contribTokenPolicyId :: CurrencySymbol,
    treasuryIssuerPolicyId :: CurrencySymbol
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''EscrowParam

data ProjectAction = Cancel | Distribute | Update
  deriving (Show)

PlutusTx.makeIsDataIndexed ''ProjectAction [('Cancel, 0), ('Distribute, 1), ('Update, 2)]
PlutusTx.makeLift ''ProjectAction

data EscrowTypes

instance ValidatorTypes EscrowTypes where
  type DatumType EscrowTypes = CommitmentEscrowDatum
  type RedeemerType EscrowTypes = ProjectAction
