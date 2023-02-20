{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module OffChain where

import PlutusTx                       qualified
import PlutusTx.Prelude               hiding (Semigroup(..), unless)

import Ledger                         hiding (singleton)
import Ledger.Constraints             qualified as Constraints
import Plutus.Script.Utils.V1.Scripts qualified as Scripts
import Ledger.Typed.Scripts
import Ledger.Ada                     as Ada
import Ledger.Value

import Plutus.Contract

import Control.Monad                  hiding (fmap)
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics                   (Generic)
import Data.Map                       as Map
import Data.Text                      (Text)
import Data.Void                      (Void)
import Prelude                        (IO, Semigroup (..), String, Float, Show, Int, readFile, print, getLine)
import Text.Printf                    (printf)

import Utils

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

toPaymentPubKeyHash :: Address -> PaymentPubKeyHash
toPaymentPubKeyHash addr = toPubKeyHash addr $ PaymentPubKeyHash . PlutusTx.Prelude.fromMaybe (error "invalid payment pub key hash")

percToAda :: Integer -> Integer
percToAda perc = totalAdaAmnt * (perc * 0.01)

multiPayBuild :: Payments -> [Constraints.TxConstraints i o]
multiPayBuild = 
    let pkey = toPaymentPubKeyHash $ walletAddress payment in
    PlutusTx.Prelude.map
      (\ payment
         -> Constraints.mustPayToPubKey pkey
             (Ada.lovelaceValueOf $ percToAda (percentage payment)))

give :: AsContractError e => Payments -> [Constraints.TxConstraints i o] -> Contract w s e () --unlock
give payments = do
    tx <- multiPayBuild payments
    ledgerTx <- submitTx tx --check what exactly to put with submitTxConstraints
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "distributed a total of %d lovelace to %d wallets" sumAda sumWal
        where sumAda = percToAda . sndList payments
              sumWal = length $ fstList payments

giveBack :: AsContractError e => Payments -> Contract w s e () --abort
giveBack = do     --user initiated, this fx will return the ada value sent to it over to the server wallet, minus fees
    let a = totalAdaAmnt
    let pkey = toPaymentPubKeyHash merchifyAdaAddress
    let tx = Constraints.mustPayToPubKey pkey $ Ada.lovelaceValueOf a --if the error originated with this last fx, this will create an infinite loop. Fix!
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" a

returnChoice :: IO ()
returnChoice = do
    response <- getLine "1 to return funds to server wallet, any other key to do nothing"
    if response == "1" then giveBack else print ""

merchifyAdaAddress :: Address
merchifyAdaAddress = "addr1q9j43yrfh5fku4a4m6cn4k3nhfy0tqupqsrvnn5mac9gklw820s3cqy4eleppdwr22ce66zjhl90xp3jv7ukygjmzdzqmzed2e"

-- totalAdaAmnt :: ScriptContext -> Integer
-- totalAdaAmnt info = PlutusTx.Prelude.foldl (\txOut -> valueOf (txOutValue txOut) "" "") 0 (txInfoOutputs info)

-- instance FromJSON Royalties where -- is this necessary if FromJSON is already derived above?
--     parseJSON (Object v) =  Royalties
--         <$> v .: "walletAddress"
--         <*> v .: "percentage"
--     parseJSON invalid = prependFailure "parsing tx output info failed"
--         (typeMismatch "Object" invalid)

-- count :: Eq a => a -> [a] -> Integer
-- count x =  length . Map.filter (==x)

-- grab :: AsContractError e => GiveParams -> Contract w s e () --not necessary in current implementation
-- grab = do
--     utxos <- utxosAt merchifyAdaAddress                                                                      -- This will find all UTXOs that sit at the script address
--     let orefs   = fst <$> Map.toList utxos                                                           -- This get all the references of the UTXOs
--         lookups = Constraints.unspentOutputs utxos      <>                                           -- Tell where to find all the UTXOS
--                   Constraints.otherScript validator                                                  -- and inform about the actual validator (the spending tx needs to provide the actual validator)
--         tx :: Constraints.TxConstraints Void Void
--         tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI 17 | oref <- orefs]  -- Define the TX giving constrains, one for each UTXO sitting on this addrs,
--                                                                                                      -- must provide a redeemer (ignored in this case)
--     ledgerTx <- submitTxConstraintsWith @Void lookups tx                                             -- Allow the wallet to construct the tx with the necesary information
--     void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                -- Wait for confirmation
--     logInfo @String $ "replenished funds"

-- grab :: forall w s e. AsContractError e => Contract w s e ()
-- grab = do
--     utxos <- utxosAt scrAddress
--     let orefs   = fst <$> Map.toList utxos
--         lookups = Constraints.unspentOutputs utxos  <>
--                   Constraints.otherScript validator
--         tx :: Constraints.TxConstraints Void Void
--         tx = mconcat [mustSpendScriptOutput oref $ arbitrary redeemer # | oref <- orefs]

--     ledgerTx <- submitTxConstraintsWith @Void lookups tx
--     void $ awaitTxConfirmed $ getCardanoTxId ledgerTx