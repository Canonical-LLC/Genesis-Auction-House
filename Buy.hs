{-# LANGUAGE NoImplicitPrelude #-}
module GenesisAuction.Buy
  ( buyCancelValidator
  , PurchaseDatum (..)
  , PurchaseRedeemer (..)
  ) where

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import           GenesisAuction.Shared



{-
This validator assumes a NFT has been locked at the script address
with the correct BuyCancellingDatum.
It supports two operations. 'Buy' which unlocks the NFT
if the correct price is paid to the seller and marketplace, or
'Cancel' which unlocks the NFT if the transaction is signed by the
Seller.

When buying, the validator assumes there is only one party
signing the transaction and this is the buyer. A possible future
improvement would be determine the output address using a different
method to support multiple signers.
-}
mkBuyCancelValidator
  :: PurchaseDatum
  -> PurchaseRedeemer
  -> ScriptContext
  -> Bool
mkBuyCancelValidator pd@PurchaseDatum {..} redemeer scriptCtx =
  let
    info :: TxInfo
    info = scriptContextTxInfo scriptCtx

  in case redemeer of
    Buy ->
      let
        input = PurchaseInput
          { piCurrencySymbol    = pdCurrencySymbol
          , piTokenName         = pdTokenName
          , piPrice             = pdPrice
          , piBuyer             = case txInfoSignatories info of
              [x] -> x
              _ -> traceError "expected one buyer to be the signer"
          , piSeller            = pdOwner
          , piFeePercentage     = pdFeePercentage
          , piFeeAddress        = pdFeeAddress
          , piRoyaltyPercentage = pdRoyaltyPercentage
          , piRoyaltyAddress    = pdRoyaltyAddress
          }
      in validatePurchase input scriptCtx
    Cancel -> validateCancel pd info

typedBuyCancelValidator :: Scripts.TypedValidator Purchasing
typedBuyCancelValidator = Scripts.mkTypedValidator @Purchasing
    $$(PlutusTx.compile [|| mkBuyCancelValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PurchaseDatum @PurchaseRedeemer

buyCancelValidator :: PlutusScript PlutusScriptV1
buyCancelValidator
  = PlutusScriptSerialised
  $ SBS.toShort
  $ LB.toStrict
  $ serialise
  $ Scripts.validatorScript
  $ typedBuyCancelValidator
