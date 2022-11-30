{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Control.Monad                  hiding (fmap)
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
        .\/ Endpoint "grab" ()

multiPayBuild :: GiveParams -> TxConstraints
multiPayBuild (GP (payment : payments)) = 
    (mustPayToPubKeyAddress (fst payment) (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf $ percToAda (snd payment)) : multiPayBuild payments

percToAda :: Float -> Int
percToAda perc = totalAdaAmnt * (perc * 0.01)

totalAdaAmnt :: TxInfo -> TxOut -> Integer --wrap this in Contract w so it can be passed to any endpoint
totalAdaAmnt TxInfo{txInfoOutputs} = case find txInfoOutputs of
    a@TxOut {txOutValue} -> logInfo @String $ printf "Recieved a total of %d lovelace" a >> return (a :: Integer)
    _ -> print "Failure retrieving total recieved ADA amount" >> returnChoice

give :: AsContractError e => GiveParams -> Contract w s e ()
give (GP payments) = do
    tx <- multiPayBuild GP payments
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "distributed a total of %d lovelace to %d wallets" sumAda sumWal
        where sumAda = sndList $ percToAda $ snd payments
              sumWal = length $ fst $ unzip payments

giveBack :: AsContractError e => GiveParams -> Contract w s e ()
giveBack = do
    let tx = mustPayToPubKeyAddress merchifyAdaAddress (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf totalAdaAmnt      --This Tx needs an output, thats its going to be the Script Address, Datum MUST be specified, so is created and the ammount of lovelaces
    ledgerTx <- submitTx tx                                                                          --This line submit the Tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                --This line waits for confirmation
    logInfo @String $ printf "made a gift of %d lovelace" amount 
    --used in case validation process fails, this fx will return the ada value sent to it back to the sender, minus fees

grab :: AsContractError e => GiveParams -> Contract w s e ()
grab = do
    utxos <- utxosAt merchifyAdaAddress                                                                      -- This will find all UTXOs that sit at the script address
    let orefs   = fst <$> Map.toList utxos                                                           -- This get all the references of the UTXOs
        lookups = Constraints.unspentOutputs utxos      <>                                           -- Tell where to find all the UTXOS
                  Constraints.otherScript validator                                                  -- and inform about the actual validator (the spending tx needs to provide the actual validator)
        tx :: TxConstraints Void Void                                                            
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI 17 | oref <- orefs]  -- Define the TX giving constrains, one for each UTXO sitting on this addrs,
                                                                                                     -- must provide a redeemer (ignored in this case)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx                                             -- Allow the wallet to construct the tx with the necesary information
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                -- Wait for confirmation
    logInfo @String $ "replenished funds"

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` giveBack' `select` grab') >> endpoints
    where
        give' = endpoint @"give" give
        giveBack' = endpoint @"giveBack" giveBack
        grab' = endpoint @"grab" grab

royaltyCheck :: Value -> Maybe [Royalties] -> Maybe IO (Bool)                          --decodes JSON and pulls the info 
royaltyCheck redeemer = do                                      --If successful, and the %s add up to 100, it saves the %s and their addresses to a list of tuples and returns true, otherwise false
    contents <- decode (readFile redeemer) :: Maybe [Royalties]
    case contents of                               
        Just contents -> case checkValues contents of
            True -> logInfo @String $ printf "validation completed, tx construction in proccess with the following parties as outputs..." >> mapM_ print contents >> give contents
            False -> logInfo @String $ printf "Royalties don't add up to 100%" >> False >> returnChoice
        _ -> logInfo @String $ printf "Royalties not formatted properly" >> print contents >> False >> returnChoice
    
checkValues :: Maybe [Royalties] -> Maybe Bool
checkValues [] = Nothing
checkValues contents = sndList contents == 100.0

count :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

sndList :: [Royalties] -> percentage
sndList royalties = foldl (\x (a,b) -> x + b) 0 royalties

returnChoice = giveBack
               -- give

merchifyAdaAddress :: Address
merchifyAdaAddress = "addr1q9j43yrfh5fku4a4m6cn4k3nhfy0tqupqsrvnn5mac9gklw820s3cqy4eleppdwr22ce66zjhl90xp3jv7ukygjmzdzqmzed2e"

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