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

import Utils

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# INLINABLE nftRoyaltyValidator #-}
nftRoyaltyValidator :: BuiltInData -> ScriptContext -> Bool
nftRoyaltyValidator _ redeemer sctx = traceIfFalse "Tx must include server wallet" txSignedBy sctx >>
        traceIfFalse "Royalty information incorrect, please reference above error msg" royaltyCheck redeemer >>
        totalAdaAmnt (info sctx)
    where
        txSignedBy :: TxInfo -> [PubKeyHash] -> Bool
        txSignedBy TxInfo{txInfoSignatories} = let m = merchifyPubKeyHash in 
            elem m txInfoSignatories

        royaltyCheck :: Data.Aeson.Value -> Maybe (IO String)     --decodes JSON and pulls the info 
        royaltyCheck redeemer = do           --If successful, and the %s add up to 100, it saves the %s and their addresses to a list of tuples and returns true, otherwise false
            contents <- decode (readFile redeemer) :: Maybe Payments
            case contents of --what's the difference between print, and putStrLn? And why is mapM_ being used instead of mapM or map?
                Just x -> if checkValues contents then logInfo @String $ "validation completed, tx construction in proccess with the following parties as outputs..." >> PlutusTx.Prelude.mapM_ print contents >> give contents
                    else logInfo @String $ "Royalties don't add up to 100%"  >> returnChoice
                Nothing -> logInfo @String $ "Royalties not formatted properly" >> print contents >> returnChoice

        info :: ScriptContext -> TxInfo
        info = scriptContextTxInfo

        checkValues :: Payments -> Bool
        checkValues [] = []
        checkValues contents = sndList contents == 100.0

        merchifyAdaAddress :: Address
        merchifyAdaAddress = "addr1q9j43yrfh5fku4a4m6cn4k3nhfy0tqupqsrvnn5mac9gklw820s3cqy4eleppdwr22ce66zjhl90xp3jv7ukygjmzdzqmzed2e"
        
        merchifyPubKeyHash :: Address -> PubKeyHash
        merchifyPubKeyHash = PlutusTx.Prelude.fromMaybe (error "invalid payment pub key hash")
            . toPubKeyHash merchifyAdaAddress


royaltyValidator :: Scripts.Validator
royaltyValidator = Scripts.mkValidatorScript 
                 $$(PlutusTx.compile [||royaltyWrapped||])                 
    where
        royaltyWrapped = wrap nftRoyaltyValidator

valHash :: ValidatorHash
valHash = Scripts.validatorHash royaltyValidator

scrAddress :: Address
scrAddress = scriptAddress valHash

serialized :: PlutusScript PlutusScriptV1
serialized = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ royaltyValidator

writeSerialized :: IO ()
writeSerialized = void $ writeFileTextEnvelope "testnet/RSCV2.plutus" Nothing serialized

writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()