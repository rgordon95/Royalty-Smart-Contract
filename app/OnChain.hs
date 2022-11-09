{-# LANGUAGE ImportQualifiedPost #-}

module OnChain where

import PlutusTx                       (Data (..))
import PlutusTx                       qualified
import PlutusTx.Prelude               hiding (Semigroup(..), unless)
import PlutusTx.Builtins              qualified   as Builtins

import Ledger                         hiding (singleton)
import Ledger.Constraints             (TxConstraints)
import Ledger.Constraints             qualified  as Constraints
import Plutus.Script.Utils.V1.Scripts qualified as Scripts --pre-Vasil is Ledger.Typed.Scripts
import Ledger.Ada                     as Ada

import Playground.Contract            (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import Playground.TH                  (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types               (KnownCurrency (..))

-- import Plutus.Contract

import Control.Monad                  hiding (fmap)
import Data.Aeson                     as JSON
import GHC.Generics                   (Generic)   
import Data.Map                       as Map
import Data.Text                      (Text)
import Data.Void                      (Void)
import Prelude                        (IO, Semigroup (..), String)
import Text.Printf                    (printf)

import OffChain                       (royaltyCheck, giveBack)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

data Royalties = Royalties {walletAddress :: String,
                            percentage :: Float} 
                            deriving (Show, FromJSON)

{-# INLINABLE mkRedeemer #-}
mkRedeemer :: JSON.Value -> ScriptContext -> Bool
mkRedeemer _ redeemer sctx = traceIfFalse "Tx must include server wallet" txSignedBy sctx
                             traceIfFalse "Royalty information incorrect, please reference above error msg" royaltyCheck redeemer                 

{-# INLINABLE txSignedBy #-}
txSignedBy :: TxInfo -> [PubKeyHash] -> Bool
txSignedBy TxInfo{txInfoSignatories} = let m = toPubKeyHash merchifyAdaAddress in 
    case find ((==) m) txInfoSignatories of
        True -> Nothing
        False -> giveBack 

{-# INLINABLE merchifyAdaAddress #-}
merchifyAdaAddress :: Address
merchifyAdaAddress = "addr1q9j43yrfh5fku4a4m6cn4k3nhfy0tqupqsrvnn5mac9gklw820s3cqy4eleppdwr22ce66zjhl90xp3jv7ukygjmzdzqmzed2e"

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance RedeemerType Typed = [Royalties]

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed 
                 $$(PlutusTx.compile [|| mkRedeemer ||])
                 $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @[Royalties] --post-Vasil is mkUntypedValidator

validator :: Validator
validator = Scripts.validatorScript typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

-- scrAddress :: Ledger.scrAddress
-- scrAddress = scriptAddress validator
