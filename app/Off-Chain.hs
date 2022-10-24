module Off-Chain where

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

type GiftSchema = 
            Endpoint "give" Integer --an integer parameter
        .\/ Endpoint "grab" ()

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount
    