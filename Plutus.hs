{-# LANGUAGE NoImplicitPrelude #-}
module GenesisAuction.Plutus
  ( buyCancelValidator
  , BuyCancellingDatum (..)
  , BuyCancelRedemeer (..)
  ) where

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import qualified Prelude as P
import qualified Ledger.Ada           as Ada
import           Plutus.V1.Ledger.Value
import           GHC.Generics
import qualified Data.Aeson as A

data BuyCancellingDatum = BuyCancellingDatum
  { bcdCurrencySymbol :: !CurrencySymbol
  -- ^ NFT Policy Id
  , bcdTokenName      :: !TokenName
  -- ^ NFT Token Name
  , bcdPrice          :: !Integer
  -- ^ Total price including any fees
  , bcdSeller         :: !PubKeyHash
  -- ^ Address of the seller
  , bcdFeePercentage  :: !Integer
  -- ^ This inverse fee percentage times 10 = (1 / percentage) * 10
  , bcdFeeAddress     :: !PubKeyHash
  -- ^ Address of the marketplace that will receive the fee
  }
  deriving (P.Show, Generic, A.ToJSON, A.FromJSON)

PlutusTx.unstableMakeIsData ''BuyCancellingDatum

data BuyCancelRedemeer = Buy | Cancel
  deriving (P.Show, Generic, A.ToJSON, A.FromJSON)

PlutusTx.unstableMakeIsData ''BuyCancelRedemeer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE feePercentToLovelaces #-}
feePercentToLovelaces :: Integer -> Integer -> Integer
feePercentToLovelaces percentage loves
  = (loves * 10) `divide` percentage

{-# INLINABLE ensureOnlyOneScriptInput #-}
ensureOnlyOneScriptInput :: ScriptContext -> Bool
ensureOnlyOneScriptInput ctx =
  let
    isScriptInput :: TxInInfo -> Bool
    isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
      Nothing -> False
      Just _ -> True
  in if length (filter isScriptInput $ txInfoInputs (scriptContextTxInfo ctx)) <= 1
       then True
       else False


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
  :: BuyCancellingDatum
  -> BuyCancelRedemeer
  -> ScriptContext
  -> Bool
mkBuyCancelValidator BuyCancellingDatum {..} redemeer scriptCtx =
  let
    info :: TxInfo
    info = scriptContextTxInfo scriptCtx

    -- At least one Ada is needed for both the seller and fee
    -- transaction output.
    minimumTransactionAda :: Integer
    minimumTransactionAda = 1000000

    buyerAddress :: PubKeyHash
    buyerAddress = case txInfoSignatories info of
      [x] -> x
      _ -> traceError "expected one buyer to be the signer"

    isNftToken :: (CurrencySymbol, TokenName, Integer) -> Bool
    isNftToken (sym, tok, theCount)
      =  sym      == bcdCurrencySymbol
      && tok      == bcdTokenName
      && theCount >= 1

    nftIsInValue :: Value -> Bool
    nftIsInValue val = case filter isNftToken $ flattenValue val of
      [_] -> True
      _   -> False

    nftInOutputForBuyer :: Bool
    nftInOutputForBuyer = nftIsInValue $ valuePaidTo info buyerAddress

    -- Must pay at least 1 Ada as fees
    minimumFees :: Integer
    minimumFees
      = max minimumTransactionAda
      $ feePercentToLovelaces bcdFeePercentage bcdPrice

    feeCheck :: Value -> Bool
    feeCheck val = lovelaces val >= minimumFees

    feesArePaid :: Bool
    feesArePaid = feeCheck $ valuePaidTo info bcdFeeAddress

    minimumSellerAmount :: Integer
    minimumSellerAmount = max minimumTransactionAda $ bcdPrice - minimumFees

    priceCheck :: Value -> Bool
    priceCheck val = lovelaces val >= minimumSellerAmount

    correctPricePaidToSeller :: Bool
    correctPricePaidToSeller = priceCheck $ valuePaidTo info bcdSeller

    -- The total price is either minimum times 2 (for seller and fee
    -- transaction) or the price if it is high enough.
    totalPriceCheck :: Value -> Bool
    totalPriceCheck val
      = lovelaces val >= max (2*minimumTransactionAda) bcdPrice

    -- This check is necessary if the seller and marketplace address could
    -- be the same
    totalEqualsPrice :: Bool
    totalEqualsPrice = if bcdSeller == bcdFeeAddress
      then totalPriceCheck $ valuePaidTo info bcdSeller
      else True

    -- Only the seller can cancel
    isSignedBySeller :: Bool
    isSignedBySeller = txSignedBy info bcdSeller

    onlyOneScriptInput :: Bool
    onlyOneScriptInput = ensureOnlyOneScriptInput scriptCtx

  in case redemeer of
    Buy -> correctPricePaidToSeller
        && nftInOutputForBuyer
        && feesArePaid
        && totalEqualsPrice
        && traceIfFalse "More than one script input" onlyOneScriptInput
    Cancel -> isSignedBySeller

data BuyCancelling
instance Scripts.ValidatorTypes BuyCancelling where
    type instance DatumType BuyCancelling = BuyCancellingDatum
    type instance RedeemerType BuyCancelling = BuyCancelRedemeer

typedBuyCancelValidator :: Scripts.TypedValidator BuyCancelling
typedBuyCancelValidator = Scripts.mkTypedValidator @BuyCancelling
    $$(PlutusTx.compile [|| mkBuyCancelValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @BuyCancellingDatum @BuyCancelRedemeer

buyCancelValidator :: PlutusScript PlutusScriptV1
buyCancelValidator
  = PlutusScriptSerialised
  $ SBS.toShort
  $ LB.toStrict
  $ serialise
  $ Scripts.validatorScript
  $ typedBuyCancelValidator
