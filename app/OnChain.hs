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

module OnChain where

import PlutusTx                       qualified
import PlutusTx.Prelude               hiding (Semigroup(..), unless)
import PlutusTx.Builtins              qualified as Builtins

import Ledger                         hiding (singleton)
import Ledger.Constraints             qualified as Constraints
import Plutus.Script.Utils.V1.Scripts qualified as Scripts --pre-Vasil is Ledger.Typed.Scripts
import Ledger.Ada                     as Ada

import Plutus.Contract

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS

import Control.Monad                  hiding (fmap)
import Data.Aeson                     
import GHC.Generics                   (Generic)   
import Data.Map                       as Map
import Data.Text                      (Text)
import Data.Void                      (Void)
import Prelude                        (IO, Semigroup (..), String, Float, Show, Int)
import Text.Printf                    (printf)

import OffChain                       (royaltyCheck)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

data Royalties = Royalties {walletAddress :: Address,
                            percentage :: Float} 
                            deriving (Show, FromJSON)

makeLift ''Royalties
makeIsDataIndexed ''Royalties [('Royalties, 0)]

{-# INLINABLE nftRoyaltyValidator #-}
nftRoyaltyValidator :: BuiltInData -> ScriptContext -> Bool
nftRoyaltyValidator _ redeemer sctx = traceIfFalse "Tx must include server wallet" txSignedBy sctx >>
                             traceIfFalse "Royalty information incorrect, please reference above error msg" royaltyCheck redeemer >>
                             totalAdaAmnt sctx
        where
            txSignedBy :: TxInfo -> [PubKeyHash] -> Bool
            txSignedBy TxInfo{txInfoSignatories} = let m = merchifyAdaAddress in 
                elem m txInfoSignatories

            info = scriptContextTxInfo sctx in
                totalAdaAmnt :: TxInfo -> TxOut -> Integer
                totalAdaAmnt = foldl (\txOut -> valueOf (txOutValue txOut) "" "") 0 (txInfoOutputs info)    --wrap this in Contract w so it can be passed to any endpoint

            merchifyAdaAddress :: Address
            merchifyAdaAddress = toPubKeyHash "addr1q9j43yrfh5fku4a4m6cn4k3nhfy0tqupqsrvnn5mac9gklw820s3cqy4eleppdwr22ce66zjhl90xp3jv7ukygjmzdzqmzed2e"

royaltyValidator :: Scripts.Validator -- do we actually need a typed validator? is this a typed validator?
royaltyValidator = Scripts.mkValidatorScript 
                 $$(PlutusTx.compile [||royaltyWrapped||])                 
    where
        royaltyWrapped = wrap nftRoyaltyValidator

validator :: Validator
validator = Scripts.validatorScript royaltyValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

serialized :: PlutusScript PlutusScriptV1
serialized = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ royaltyValidator

writeSerialized :: IO ()
writeSerialized = void $ writeFileTextEnvelope "testnet/RSCV2.plutus" Nothing serialized

writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()
