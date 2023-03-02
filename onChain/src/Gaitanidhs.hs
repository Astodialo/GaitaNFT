{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Gaitanidhs where

import           Cardano.Api                                           (PlutusScript,
                                                                        PlutusScriptV2,
                                                                        writeFileTextEnvelope)
import           Cardano.Api.Shelley                                   (PlutusScript (..),
                                                                        ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                                        fromPlutusData,
                                                                        scriptDataToJson)
import           Codec.Serialise
import           Data.Aeson                                            as A
import qualified Data.ByteString.Lazy                                  as LBS
import qualified Data.ByteString.Short                                 as SBS
import           Data.Functor                                          (void)
import           GHC.Generics                                          (Generic)

import           Ledger
import           Ledger.Constraints                                    as Constraints
import           Ledger.Typed.Scripts                                  as LScripts
import           Ledger.Value                                          as Value

import qualified Plutus.Script.Utils.V2.Scripts                        as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts                  as TScripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as Scripts
import           Plutus.V1.Ledger.Address                              as Addr
import           Plutus.V1.Ledger.Value                                as V
import qualified Plutus.V2.Ledger.Api                                  as Api
import           Plutus.V2.Ledger.Contexts                             as Api

import           PlutusTx
import           PlutusTx.Prelude                                      as Plutus hiding
                                                                                 (Semigroup (..),
                                                                                  unless,
                                                                                  (.))

import           Data.Text                                             (Text)
import           Prelude                                               (FilePath,
                                                                        IO,
                                                                        Semigroup (..),
                                                                        Show (..),
                                                                        String,
                                                                        print,
                                                                        (.))

{-# INLINEABLE mintPolicy #-}
mintPolicy :: Api.TxOutRef -> BuiltinData -> Api.ScriptContext -> Bool
mintPolicy oref _ ctx = traceIfFalse "wrong UTxO" checkUTxOConsumed

  where
    txInfo :: Api.TxInfo
    txInfo = Api.scriptContextTxInfo ctx

    checkUTxOConsumed :: Bool
    checkUTxOConsumed = any (\utxo -> Api.txInInfoOutRef utxo == oref ) (Api.txInfoInputs txInfo)

policy :: Api.TxOutRef -> Scripts.MintingPolicy
policy oref = Api.mkMintingPolicyScript $ $$(PlutusTx.compile [|| wrap ||])
                                        `PlutusTx.applyCode`
                                        PlutusTx.liftCode oref
              where
                wrap oref' =  TScripts.mkUntypedMintingPolicy $ mintPolicy oref'

script :: Api.Script
script = Api.unMintingPolicyScript $ policy (Api.TxOutRef {txOutRefId = "d045f22af35bbad0b741d93733567b5756b66a1bc90b9d1300fab787ea8724fc", txOutRefIdx = 0})

{- As a Short Byte String -}

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

{- As a Serialised Script -}

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "nftMintV2.plutus" Nothing serialisedScript

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . A.encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . Api.toData

