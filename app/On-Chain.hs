module Main where

import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins    as Builtins

import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts --Plutus.Script.Utils.V1.Scripts
import           Ledger.Ada           as Ada

import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))

import           Plutus.Contract

import           Control.Monad       hiding (fmap)
import           Data.Aeson          (ToJSON, FromJSON)
import           GHC.Generics        (Generic)   
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)
import           On-Chain

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

data royalties = Royalties {
    nftPercentage :: (String, Float)
    partnerPercentage :: (String, Float)
} deriving Show, Generic

{-# INLINABLE redeemer #-}
eitherRedeemer :: String -> Maybe royalties -> ScriptContext -> Either Bool String
eitherRedeemer datum redeemer _ = traceIfFalse "Invalid address supplied to redeemer!" (all isHexDigit address)
redeemer 

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = String
    type instance RedeemerType Typed = Maybe royalties

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed 
                 $$(PlutusTx.compile [|| redeemer ||])
                 $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @Maybe royalties @String

validator :: Validator
validator = Scripts.validatorScript typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.scrAddress
scrAddress = scriptAddress validator

{-# INLINABLE  #-}

