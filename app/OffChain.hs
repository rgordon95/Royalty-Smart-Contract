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
merchifyAdaAddress :: Address
merchifyAdaAddress = "addr1q9j43yrfh5fku4a4m6cn4k3nhfy0tqupqsrvnn5mac9gklw820s3cqy4eleppdwr22ce66zjhl90xp3jv7ukygjmzdzqmzed2e"
--Takes the Address above and turns it 
toPaymentPubKeyHash :: Address -> PaymentPubKeyHash
toPaymentPubKeyHash addr = toPubKeyHash addr $ PaymentPubKeyHash . 
        PlutusTx.Prelude.fromMaybe (error "invalid payment pub key hash")
--Turns a % share into an actual Ada amount
percToAda :: Integer -> Integer
percToAda perc = totalAdaAmnt * (perc * 0.01)
--Creates the TxConstraints to send payment to multiple recipient wallets
multiPayBuild :: Payments -> [Constraints.TxConstraints i o]
multiPayBuild = 
    let pkey = toPaymentPubKeyHash $ walletAddress payment in
    PlutusTx.Prelude.map
      (\ payment
         -> Constraints.mustPayToPubKey pkey
             (Ada.lovelaceValueOf $ percToAda (percentage payment)))
give :: AsContractError e => Payments -> 
        [Constraints.TxConstraints i o] -> 
            Contract w s e () --unlock
give payments = do
    tx <- multiPayBuild payments
    ledgerTx <- submitTx tx --check what exactly to put with submitTxConstraints
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "distributed a total of %d lovelace to %d wallets" sumAda sumWal
        where sumAda = percToAda . sndList payments
              sumWal = length $ fstList payments
--user initiated, this fx will return the ada value sent to it over to the server wallet, minus fees
giveBack :: AsContractError e => Payments -> Contract w s e ()
giveBack = do
    let a = totalAdaAmnt
    let pkey = toPaymentPubKeyHash merchifyAdaAddress
    let tx = Constraints.mustPayToPubKey pkey $ Ada.lovelaceValueOf a
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" a
--called after any error occurs, gives user the choice whether to return funds or not
returnChoice :: IO ()
returnChoice = do
    response <- getLine "1 to return funds to server wallet, any other key to do nothing"
    if response == "1" then giveBack else print ""