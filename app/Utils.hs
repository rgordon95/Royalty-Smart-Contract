{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}

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

totalAdaAmnt :: TxInfo -> Integer 
totalAdaAmnt info = 
    let outputs = txInfoOutputs info
        valueSum = foldMap (Ledger.txOutValue . Ledger.TxOut) outputs
    in valueOf valueSum Ada.adaSymbol Ada.adaToken

fstList :: Payments -> [Address]
fstList = map walletAddress

sndList :: Payments -> [P.Float]
sndList = map percentage