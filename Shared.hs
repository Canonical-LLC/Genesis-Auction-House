{-# LANGUAGE NoImplicitPrelude #-}
module GenesisAuction.Shared
  ( validatePurchase
  , validateCancel
  , PurchaseDatum (..)
  , PurchaseRedeemer (..)
  , PurchaseInput (..)
  , Purchasing
  , lovelaces
  ) where

import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import qualified Prelude as P
import qualified Ledger.Ada           as Ada
import           Plutus.V1.Ledger.Value
import           GHC.Generics

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

data PurchaseDatum = PurchaseDatum
  { pdCurrencySymbol :: !CurrencySymbol
  -- ^ NFT Policy Id
  , pdTokenName      :: !TokenName
  -- ^ NFT Token Name
  , pdPrice          :: !Integer
  -- ^ Total price including any fees
  , pdOwner          :: !PubKeyHash
  -- ^ Address of the asset owner
  , pdFeePercentage  :: !Integer
  -- ^ This inverse fee percentage times 10 = (1 / percentage) * 10
  , pdFeeAddress     :: !PubKeyHash
  -- ^ Address of the marketplace that will receive the fee
  , pdRoyaltyPercentage :: !Integer
  -- ^ Royalty inverse percentage times 10 = (1 / percentage) * 10
  , pdRoyaltyAddress :: !PubKeyHash
  -- ^ Address of the royalty user that will receive the fee
  }
  deriving (P.Show, Generic)

PlutusTx.unstableMakeIsData ''PurchaseDatum

data PurchaseRedeemer = Buy | Cancel
  deriving (P.Show, Generic)

PlutusTx.unstableMakeIsData ''PurchaseRedeemer

data PurchaseInput = PurchaseInput
  { piCurrencySymbol :: !CurrencySymbol
  -- ^ NFT Policy Id
  , piTokenName      :: !TokenName
  -- ^ NFT Token Name
  , piPrice          :: !Integer
  -- ^ Total price including any fees
  , piBuyer          :: !PubKeyHash
  -- ^ Address of the buyer
  , piSeller         :: !PubKeyHash
  -- ^ Address of the seller
  , piFeePercentage  :: !Integer
  -- ^ This inverse fee percentage times 10 = (1 / percentage) * 10
  , piFeeAddress     :: !PubKeyHash
  -- ^ Address of the marketplace that will receive the fee
  , piRoyaltyPercentage :: !Integer
  -- ^ Royalty inverse percentage times 10 = (1 / percentage) * 10
  , piRoyaltyAddress :: !PubKeyHash
  -- ^ Address of the royalty user that will receive the fee
  }

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE feePercentToLovelaces #-}
feePercentToLovelaces :: Integer -> Integer -> Integer
feePercentToLovelaces percentage loves
  = (loves * 10) `divide` percentage

{-# INLINABLE validateCancel #-}
validateCancel :: PurchaseDatum -> TxInfo -> Bool
validateCancel PurchaseDatum {..} info = txSignedBy info pdOwner

{-# INLINABLE lovelacesPaidTo #-}
lovelacesPaidTo :: TxInfo -> PubKeyHash -> Integer
lovelacesPaidTo info pkh = lovelaces (valuePaidTo info pkh)

{-# INLINABLE validatePurchase #-}
validatePurchase :: PurchaseInput -> ScriptContext -> Bool
validatePurchase PurchaseInput {..} scriptCtx =
  let
    info :: TxInfo
    info = scriptContextTxInfo scriptCtx
    -- At least one Ada is needed for both the seller and fee
    -- transaction output.
    minimumTransactionAda :: Integer
    minimumTransactionAda = 1000000

    nftInOutputForBuyer :: Bool
    nftInOutputForBuyer
      = valueOf (valuePaidTo info piBuyer) piCurrencySymbol piTokenName >= 1

    -- Must pay at least 1 Ada as fees
    marketplaceFee :: Integer
    marketplaceFee
      = max minimumTransactionAda
      $ feePercentToLovelaces piFeePercentage piPrice

    feesArePaid :: Bool
    feesArePaid = lovelacesPaidTo info piFeeAddress >= marketplaceFee

    royalty :: Integer
    royalty
      = if piRoyaltyPercentage == 0
        then 0
        else max minimumTransactionAda
          $ feePercentToLovelaces piRoyaltyPercentage piPrice

    royaltiesArePaid :: Bool
    royaltiesArePaid = lovelacesPaidTo info piRoyaltyAddress >= royalty

    sellerAmount :: Integer
    sellerAmount
      = max minimumTransactionAda
      $ (piPrice - if piSeller /= piFeeAddress then marketplaceFee else 0)
      - if piSeller /= piRoyaltyAddress then royalty else 0

    correctPricePaidToSeller :: Bool
    correctPricePaidToSeller = lovelacesPaidTo info piSeller >= sellerAmount

    onlyOneScriptInput :: Bool
    onlyOneScriptInput = ensureOnlyOneScriptInput scriptCtx

  in traceIfFalse "Seller not paid" correctPricePaidToSeller
  && traceIfFalse "Buyer missing NFT" nftInOutputForBuyer
  && traceIfFalse "Fees are not paid" feesArePaid
  && traceIfFalse "Royalties are not paid" royaltiesArePaid
  && traceIfFalse "More than one script input" onlyOneScriptInput

data Purchasing
instance Scripts.ValidatorTypes Purchasing where
    type instance DatumType Purchasing = PurchaseDatum
    type instance RedeemerType Purchasing = PurchaseRedeemer
