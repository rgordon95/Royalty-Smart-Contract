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

import PlutusTx                       qualified
import PlutusTx.Prelude               hiding (Semigroup(..), unless)
import PlutusTx.Builtins              qualified as Builtins

import Ledger                         hiding (singleton)
import Ledger.Constraints             qualified as Constraints
import Plutus.Script.Utils.V1.Scripts qualified as Scripts --pre-Vasil is Ledger.Typed.Scripts
import Ledger.Ada                     as Ada

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

data Royalties = Royalties {walletAddress :: Address,
                            percentage :: Float}
                            deriving (Show, FromJSON)

instance FromJSON Royalties where
    parseJSON (Object v) =  Royalties
        <$> v .: "walletAddress"
        <*> v .: "percentage"
    parseJSON invalid = prependFailure "parsing tx output info failed"
        (typeMismatch "Object" invalid)

newtype GiveParams = GP {payments :: [Royalties]}
                     deriving (Generic)

multiPayBuild :: GiveParams -> Constraints.TxConstraints
multiPayBuild (GP (payment : payments)) =
    mustPayToPubKeyAddress (fst payment) (Datum $ Builtins.mkI 0) (Ada.lovelaceValueOf $ percToAda (snd payment)) : multiPayBuild payments

percToAda :: Float -> Int
percToAda perc = totalAdaAmnt * (perc * 0.01)

give :: AsContractError e => GiveParams -> Contract w s e () --unlock
give (GP payments) = do
    tx <- multiPayBuild GP payments
    ledgerTx <- submitTxConstraints typedValidator tx --check what exactly to put with submitTxConstraints
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "distributed a total of %d lovelace to %d wallets" sumAda sumWal
        where sumAda = sndList $ percToAda $ snd payments
              sumWal = length $ map fst payments

giveBack :: AsContractError e => GiveParams -> Contract w s e () --abort
giveBack = do     --user initiated, this fx will return the ada value sent to it over to the server wallet, minus fees
    let tx = mustPayToPubKeyAddress merchifyAdaAddress (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf totalAdaAmnt --if the error originated with this last fx, this will create an infinite loop. Fix!
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

royaltyCheck :: Value -> Maybe [Royalties] -> Maybe IO Bool                          --decodes JSON and pulls the info 
royaltyCheck redeemer = do           --If successful, and the %s add up to 100, it saves the %s and their addresses to a list of tuples and returns true, otherwise false
    contents <- decode (readFile redeemer) :: Maybe [Royalties]
    case contents of --what's the difference between logInfo @type, printf, print, and putStrLn? And why is mapM_ being used instead of mapM or map?
        Just contents -> if checkValues contents then logInfo @String $ printf "validation completed, tx construction in proccess with the following parties as outputs..." >> mapM_ print contents >> give contents
            else logInfo @String $ printf "Royalties don't add up to 100%" >> False >> returnChoice
        _ -> logInfo @String $ printf "Royalties not formatted properly" >> print contents >> False >> returnChoice

checkValues :: Maybe [Royalties] -> Maybe Bool
checkValues [] = Nothing
checkValues contents = sndList contents == 100.0

count :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

sndList :: [Royalties] -> percentage
sndList = foldl (\x (a,b) -> x + b) 0

returnChoice :: IO ()
returnChoice = do
    response <- getChar "1 to return funds to server wallet, 2 to do nothing"
    case response of
        "1" -> giveBack
        _ -> Nothing

merchifyAdaAddress :: Address
merchifyAdaAddress = "addr1q9j43yrfh5fku4a4m6cn4k3nhfy0tqupqsrvnn5mac9gklw820s3cqy4eleppdwr22ce66zjhl90xp3jv7ukygjmzdzqmzed2e"

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