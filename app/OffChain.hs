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


{-# OPTIONS_GHC -fno-warn-unused-imports #-}

data Royalties = Royalties {walletAddress :: String,
                            percentage :: Float} 
                            deriving (Show, FromJSON)

instance FromJSON Royalties where
    parseJSON (Object v) =  Royalties
        <$> v .: "walletAddress"
        <*> v .: "percentage"
    parseJSON invalid = prependFailure "parsing tx output info failed"
        (typeMismatch "Object" invalid)

data GiveParams = GP {payments :: [Royalties]}
                     deriving (Generic, ToSchema)

type GiftSchema = 
            Endpoint "give" GiveParams
        .\/ Endpoint "giveBack" ()

multiPayBuild :: GiveParams -> ScriptContext -> TxConstraints
multiPayBuild (GP (payment : payments)) = 
    (mustPayToPubKeyAddress (fst payment) (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf $ percToAda (snd payment)) : multiPayBuild payments
    where percToAda perc = totalAdaAmnt sctx * (perc * 0.01)

totalAdaAmnt :: TxInfo -> TxOut -> Int
totalAdaAmnt TxInfo{txInfoOutputs} = case find txInfoOutputs of
    a@TxOut {txOutValue} -> logInfo @String $ printf "Recieved a total of %d lovelace" a >> return (a :: Int)
    _ -> print "Failure retrieving total recieved ADA amount" >> giveBack
    --still need to figure out how to save the ada amount sent to the scr addr as an Int to use for royalty calculations. 
    --Also figure out if this fx needs to be inlinable.

give :: AsContractError e => GiveParams -> Contract w s e ()
give (GP payments) = do
    tx <- multiPayBuild (GP payments)
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "distributed a total of %d lovelace to %d wallets" sumAda sumWal
        where sumAda = fmap sum (fst payments)
              sumWal = fmap count (snd payments) --this might throw an error, may have to do explicit recursion

giveBack :: AsContractError e => GiveParams -> Contract w s e ()
giveBack = do
    --used in case validation process fails, this fx will return the ada value sent to it back to the sender, minus fees

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
    where
        give' = endpoint @"give" give
        giveBack' = endpoint @"giveBack" $ giveBack

royaltyCheck :: Value -> Maybe [Royalties] -> IO (Bool)                          --decodes JSON and pulls the info 
royaltyCheck redeemer = do                                      --If successful, and the %s add up to 100, it saves the %s and their addresses to a list of tuples and returns true, otherwise false
    contents <- decode (readFile redeemer) :: Maybe [Royalties]
    case contents of                               
        Just contents -> case checkValues contents of
            True -> logInfo @String $ printf "validation completed, tx construction in proccess with the following parties as outputs..." >> mapM_ print contents >> give contents
            False -> logInfo @String $ printf "Royalties don't add up to 100%, returning funds..." >> False >> giveBack
        _ -> logInfo @String $ printf "Royalties not formatted properly, returning funds..." >> print contents >> False >> giveBack
    
checkValues :: Maybe [Royalties] -> Bool
checkValues [] = Nothing
checkValues contents = 
    let adding = foldl (\x (a,b) -> x + b) 0 in 
        adding contents == 100.0

mkSchemaDefinitions ''GiftSchema
mkKnownCurrencies [] --Playground specific, allows tADA or any custom defined asset to be used in simulations

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