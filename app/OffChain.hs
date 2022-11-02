{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}

module OffChain where

import PlutusTx                       (Data (..))
import PlutusTx                       qualified
import PlutusTx.Prelude               hiding (Semigroup(..), unless)
import PlutusTx.Builtins              qualified as Builtins

import Ledger                         hiding (singleton)
import Ledger.Constraints             (TxConstraints)
import Ledger.Constraints             qualified as Constraints
import Plutus.Script.Utils.V1.Scripts qualified as Scripts --pre-Vasil is Ledger.Typed.Scripts
import Ledger.Ada                     as Ada

import Playground.Contract            (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import Playground.TH                  (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types               (KnownCurrency (..))

import Plutus.Contract

import Control.Monad                  --hiding (fmap)
import Data.Aeson                     
import GHC.Generics                   (Generic)   
import Data.Map                       as Map
import Data.Text                      (Text)
import Data.Void                      (Void)
import Prelude                        (IO, Semigroup (..), String)
import Text.Printf                    (printf)

import OnChain

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

instance FromJSON royalties where
    parseJSON (Object v) =  Royalties
        <$> v .: "walletAddress"
        <*> v .: "percentage"
    parseJSON invalid = prependFailure "parsing tx output info failed"
        (typeMismatch "Object" invalid)

data GiveParams = GP {payments :: [Royalties]}
                     , deriving (Generic, ToSchema)

type GiftSchema = 
            Endpoint "give" GiveParams
        -- .\/ Endpoint "grab" ()

multiPayBuild :: GiveParams -> TxConstraints
multiPayBuild (GP (payment : payments)) = 
    (mustPayToPubKeyAddress (fst payment) (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf $ percToAda (snd payment)) tx payments
    where percToAda perc = totalAdaAmnt * (perc * .01)

totalAdaAmnt :: TxInfo -> TxOut -> Value
totalAdaAmnt TxInfo{txInfoOutputs} = case find txInfoOutputs of
    a@TxOut {txOutValue} -> logInfo @String $ printf "Recieved a total of %d lovelace" a >> 
    --still need to figure out how to save the ada amount sent to the scr addr as an Int to use for royalty calculations. Also figure out if this fx needs to be inlinable.
    _ -> print "Failure retrieving total recieved ADA amount"

give :: AsContractError e => GiveParams -> Contract w s e ()
give (GP payments) = do
    tx <- multiPayBuild (GP payments)
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "distributed a total of %d lovelace to %d wallets" sumAda sumWal
        where sumAda = fmap sum (fst payments)
              sumWal = fmap count (snd payments) --this might throw an error, may have to do explicit recursion

-- grab :: forall w s e. AsContractError e => Contract w s e ()
-- grab = do
--     utxos <- utxosAt scrAddress
--     let orefs   = fst <$> Map.toList utxos
--         lookups = Constraints.unspentOutputs utxos  <>
--                   Constraints.otherScript validator
--         tx :: TxConstraints Void Void
--         tx = mconcat [mustSpendScriptOutput oref $ arbitrary redeemer # | oref <- orefs]

--     ledgerTx <- submitTxConstraintsWith @Void lookups tx
--     void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
    where
        give' = endpoint @"give" give
        -- grab' = endpoint @"grab" $ const grab

royaltyCheck :: Value -> Maybe [Royalties] -> IO (Bool)                          --decodes JSON and pulls the info 
royaltyCheck redeemer = do                                      --If successful, and the %s add up to 100, it saves the %s and their addresses to a list of tuples and returns true, otherwise false
    contents <- decode (readFile redeemer) :: Maybe [Royalties]
    case contents of                               
        Just contents -> checkValues contents >> mapM_ print (outputs :: [Royalties])
        _ -> print contents >> False
    
    
checkValues :: Maybe [Royalties] -> Bool
checkValues contents = do



-- if map . sum $ snd $ contents = 100 then True else False



mkSchemaDefinitions ''GiftSchema
mkKnownCurrencies [] --Playground specific, allows tADA or any custom defined asset to be used in simulations
