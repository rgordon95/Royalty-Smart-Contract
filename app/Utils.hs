{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils where

import PlutusTx                       (makeIsDataIndexed)
import PlutusTx.Prelude               as Tx hiding (Semigroup(..), unless)
import Data.Aeson                     


{-# OPTIONS_GHC -fno-warn-unused-imports #-}

data Royalties = Royalties {walletAddress :: Address,
                            percentage :: Float}
                            deriving (Show, Generic, FromJSON)

PlutusTx.makeIsDataIndexed ''Royalties [('Royalties, 0)]

type Payments = [Royalties]

totalAdaAmnt :: TxInfo -> Integer 
totalAdaAmnt info = 
    let outputs = txInfoOutputs info
        valueSum = Tx.foldMap txOutValue outputs
    in valueOf valueSum "" ""

fstList :: Payments -> [Address]
fstList = Tx.map walletAddress

sndList :: Payments -> [Float]
sndList = Tx.map percentage