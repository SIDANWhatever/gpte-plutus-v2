module SidanAPI where

import Prelude (IO, ($), (.))
import qualified SIDANPlutusServer            as SIDAN
import qualified GPTE.EscrowValidator         as EV
import qualified GPTE.TreasuryValidator       as TV

main :: IO ()
main = SIDAN.createServer app


app :: SIDAN.Api
app = do
  SIDAN.createEndpoint "gpte-escrow" $ SIDAN.mkV2Validator EV.validator
  SIDAN.createEndpoint "gpte-treasury" $ SIDAN.mkV2Validator TV.validator
  SIDAN.createEndpoint "gpte-treasury-custom" $ SIDAN.mkV2Validator TV.customValidator