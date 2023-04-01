{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module Utils where

import Prelude                        qualified as P

import PlutusTx                       (makeIsDataIndexed)
import PlutusTx.Prelude               as Tx hiding (Semigroup(..), unless)
import PlutusTx.Builtins              

import Ledger                         hiding (singleton)
import Ledger.Constraints             qualified as Constraints
import Plutus.Script.Utils.V1.Scripts qualified as Scripts
import Ledger.Typed.Scripts
import Ledger.Ada                     as Ada
import Ledger.Value
import Plutus.V1.Ledger.Tx

import Plutus.Contract
import Data.Aeson                     
import GHC.Generics                   (Generic)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

data Royalties = Royalties {walletAddress :: Address,
                            percentage :: P.Float}
                            deriving (P.Show, Generic, FromJSON)

PlutusTx.makeIsDataIndexed ''Royalties [('Royalties, 0)]

type Payments = [Royalties]
--Get the total amount of Ada sent to contract in previous Tx
totalAdaAmnt :: TxInfo -> Integer 
totalAdaAmnt TxInfo{txInfoOutputs} = 
    let valueSum = foldMap (Ledger.txOutValue . Ledger.TxOut) txInfoOutputs
    in valueOf valueSum Ada.adaSymbol Ada.adaToken
--Save all the Addresses and % shares to separate lists
fstList :: Payments -> [Address]
fstList = map walletAddress

sndList :: Payments -> [P.Float]
sndList = map percentage