{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | User-friendly pretty-printing for textual user interfaces (TUI)
module Cardano.CLI.Run.Friendly (friendlyTxBodyBS) where

import           Cardano.Prelude
import qualified Prelude

import           Data.Aeson (Value (..), object, toJSON, (.=))
import           Data.Yaml (array)
import           Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare)

import           Cardano.Api as Api (AddressInEra (..),
                   AddressTypeInEra (ByronAddressInAnyEra, ShelleyAddressInEra),
                   IsCardanoEra (cardanoEra), TxAuxScripts (..), TxBody, TxBodyContent (..),
                   TxCertificates (..), TxFee (..), TxIn, TxMetadata (..), TxMetadataInEra (..),
                   TxMetadataValue (..), TxMintValue (..), TxOut (..), TxOutValue (..),
                   TxUpdateProposal (..), TxValidityLowerBound (..), TxValidityUpperBound (..),
                   TxWithdrawals (..),
                   ValidityUpperBoundSupportedInEra (ValidityUpperBoundInShelleyEra), ViewTx,
                   auxScriptsSupportedInEra, certificatesSupportedInEra, displayError,
                   getTransactionBodyContent, multiAssetSupportedInEra, serialiseAddress,
                   serialiseAddressForTxOut, serialiseToRawBytesHexText, txMetadataSupportedInEra,
                   updateProposalSupportedInEra, validityLowerBoundSupportedInEra,
                   validityUpperBoundSupportedInEra, withdrawalsSupportedInEra)
import           Cardano.Api.Byron (Lovelace (..))
import           Cardano.Api.Shelley (Address (ShelleyAddress), Certificate (..), StakeAddress (..),
                   StakePoolParameters (..))
import qualified Shelley.Spec.Ledger.API as Shelley

import           Cardano.CLI.Helpers (textShow)

friendlyTxBodyBS
  :: IsCardanoEra era => Api.TxBody era -> Either Prelude.String ByteString
friendlyTxBodyBS =
  fmap (encodePretty $ setConfCompare compare defConfig) . friendlyTxBody

friendlyTxBody
  :: forall era
  .  IsCardanoEra era => Api.TxBody era -> Either Prelude.String Value
friendlyTxBody txbody =
  case getTransactionBodyContent txbody of
    Right
      TxBodyContent
        { txIns
        , txOuts
        , txFee
        , txValidityRange
        , txMetadata
        , txAuxScripts
        , txWithdrawals
        , txCertificates
        , txUpdateProposal
        , txMintValue
        } ->
      Right $
      object
        $   [ "era"     .= era
            , "fee"     .= friendlyFee txFee
            , "inputs"  .= friendlyInputs txIns
            , "outputs" .= map friendlyTxOut txOuts
            ]
        ++  [ "auxiliary scripts" .= friendlyAuxScripts txAuxScripts
            | Just _ <- [auxScriptsSupportedInEra era]
            ]
        ++  [ "certificates" .= friendlyCertificates txCertificates
            | Just _ <- [certificatesSupportedInEra era]
            ]
        ++  [ "metadata" .= friendlyMetadata txMetadata
            | Just _ <- [txMetadataSupportedInEra era]
            ]
        ++  [ "mint" .= friendlyMintValue txMintValue
            | Right _ <- [multiAssetSupportedInEra era]
            ]
        ++  [ "update proposal" .= friendlyUpdateProposal txUpdateProposal
            | Just _ <- [updateProposalSupportedInEra era]
            ]
        ++  friendlyValidityRange txValidityRange
        ++  [ "withdrawals" .= friendlyWithdrawals txWithdrawals
            | Just _ <- [withdrawalsSupportedInEra era]
            ]
    Left err -> Left $ displayError err
  where
    era = cardanoEra @era

friendlyValidityRange
  :: forall era
  .  IsCardanoEra era
  => (TxValidityLowerBound era, TxValidityUpperBound era) -> [(Text, Value)]
friendlyValidityRange = \case
  ( TxValidityNoLowerBound,
    TxValidityUpperBound ValidityUpperBoundInShelleyEra ttl
    ) ->
      -- special case: in Shelley, upper bound is TTL, and no lower bound
      ["time to live" .= ttl]
  (lowerBound, upperBound) ->
    [ "validity range" .=
        object
          ( [ "lower bound" .=
                case lowerBound of
                  TxValidityNoLowerBound   -> Null
                  TxValidityLowerBound _ s -> toJSON s
            | isLowerBoundSupported
            ]
          ++
            [ "upper bound" .=
                case upperBound of
                  TxValidityNoUpperBound _ -> Null
                  TxValidityUpperBound _ s -> toJSON s
            | isUpperBoundSupported
            ]
          )
    | isLowerBoundSupported || isUpperBoundSupported
    ]
  where
    era = cardanoEra @era
    isLowerBoundSupported = isJust $ validityLowerBoundSupportedInEra era
    isUpperBoundSupported = isJust $ validityUpperBoundSupportedInEra era

friendlyWithdrawals :: TxWithdrawals ViewTx era -> Value
friendlyWithdrawals TxWithdrawalsNone = Null
friendlyWithdrawals (TxWithdrawals _ withdrawals) =
  array
    [ object $
        "address"     .= serialiseAddress addr   :
        "amount"      .= friendlyLovelace amount :
        friendlyStakeAddress addr
    | (addr, amount, _) <- withdrawals
    ]

friendlyStakeAddress :: StakeAddress -> [(Text, Value)]
friendlyStakeAddress (StakeAddress net cred) =
  ["network" .= net, "credential" .= cred]

friendlyTxOut :: TxOut era -> Value
friendlyTxOut (TxOut addr amount) =
  case addr of
    AddressInEra ByronAddressInAnyEra _ ->
      object $ ("address era" .= String "Byron") : common
    AddressInEra (ShelleyAddressInEra _) (ShelleyAddress net cred stake) ->
      object $
        "address era"         .= String "Shelley"             :
        "network"             .= net                          :
        "payment credential"  .= cred                         :
        "stake reference"     .= friendlyStakeReference stake :
        common
  where
    common :: [(Text, Value)]
    common =
      [ "address" .= serialiseAddressForTxOut addr
      , "amount"  .= friendlyTxOutValue amount
      ]

friendlyStakeReference :: Shelley.StakeReference crypto -> Value
friendlyStakeReference = \case
  Shelley.StakeRefBase cred -> toJSON cred
  Shelley.StakeRefNull -> Null
  Shelley.StakeRefPtr ptr -> toJSON ptr

friendlyUpdateProposal :: TxUpdateProposal era -> Value
friendlyUpdateProposal = \case
  TxUpdateProposalNone -> Null
  TxUpdateProposal _ p -> String $ textShow p

friendlyCertificates :: TxCertificates ViewTx era -> Value
friendlyCertificates = \case
  TxCertificatesNone    -> Null
  TxCertificates _ cs _ -> array $ map friendlyCertificate cs

friendlyCertificate :: Certificate -> Value
friendlyCertificate =
  object .
  \case
    -- Stake address certificates
    StakeAddressRegistrationCertificate credential ->
      [ "Type"       .= String "stake address registraction"
      , "credential" .= credential
      ]
    StakeAddressDeregistrationCertificate credential ->
      [ "Type"       .= String "stake address deregistraction"
      , "credential" .= credential
      ]
    StakeAddressDelegationCertificate credential poolId ->
      [ "Type"       .= String "stake address delegation"
      , "credential" .= credential
      , "pool"       .= poolId
      ]

    -- Stake pool certificates
    StakePoolRegistrationCertificate parameters ->
      "Type" .= String "stake pool registration" :
      friendlyStakePoolParameters parameters
    -- StakePoolRetirementCertificate   PoolId EpochNo

    -- Special certificates
    GenesisKeyDelegationCertificate genesisKeyHash delegateKeyHash vrfKeyHash ->
      [ "Type"              .= String "genesis key delegation"
      , "genesis key hash"  .= serialiseToRawBytesHexText genesisKeyHash
      , "delegate key hash" .= serialiseToRawBytesHexText delegateKeyHash
      , "VRF key hash"      .= serialiseToRawBytesHexText vrfKeyHash
      ]
    -- MIRCertificate MIRPot MIRTarget

friendlyStakePoolParameters :: StakePoolParameters -> [(Text, Value)]
friendlyStakePoolParameters
  StakePoolParameters
    { stakePoolId
    , stakePoolVRF
    , stakePoolCost
    , stakePoolMargin
    , stakePoolRewardAccount
    , stakePoolPledge
    , stakePoolOwners
    , stakePoolRelays
    , stakePoolMetadata
    } =
  [ "pool"            .= stakePoolId
  , "VRF key hash"    .= serialiseToRawBytesHexText stakePoolVRF
  , "cost"            .= friendlyLovelace stakePoolCost
  , "margin"          .= friendlyRational stakePoolMargin
  , "reward account"  .= object (friendlyStakeAddress stakePoolRewardAccount)
  , "pledge"          .= friendlyLovelace stakePoolPledge
  , "owners (stake key hashes)"
                      .= map serialiseToRawBytesHexText stakePoolOwners
  -- , "relays"
  ]

friendlyRational :: Rational -> Value
friendlyRational r =
  String $
    case d of
      1 -> textShow n
      _ -> textShow n <> "/" <> textShow d
  where
    n = numerator r
    d = denominator r

friendlyFee :: TxFee era -> Value
friendlyFee = \case
  TxFeeImplicit _     -> "implicit"
  TxFeeExplicit _ fee -> friendlyLovelace fee

friendlyLovelace :: Lovelace -> Value
friendlyLovelace (Lovelace value) = String $ textShow value <> " Lovelace"

friendlyMintValue :: TxMintValue ViewTx era -> Value
friendlyMintValue = \case
  TxMintNone        -> Null
  TxMintValue _ v _ -> toJSON v

friendlyTxOutValue :: TxOutValue era -> Value
friendlyTxOutValue = \case
  TxOutAdaOnly _ lovelace -> friendlyLovelace lovelace
  TxOutValue _ multiasset -> toJSON multiasset

friendlyMetadata :: TxMetadataInEra era -> Value
friendlyMetadata = \case
  TxMetadataNone                   -> Null
  TxMetadataInEra _ (TxMetadata m) -> toJSON $ friendlyMetadataValue <$> m

friendlyMetadataValue :: TxMetadataValue -> Value
friendlyMetadataValue = \case
  TxMetaNumber int   -> toJSON int
  TxMetaBytes  bytes -> String $ textShow bytes
  TxMetaList   lst   -> array $ map friendlyMetadataValue lst
  TxMetaMap    m     ->
    array
      [array [friendlyMetadataValue k, friendlyMetadataValue v] | (k, v) <- m]
  TxMetaText   text  -> toJSON text

friendlyAuxScripts :: TxAuxScripts era -> Value
friendlyAuxScripts = \case
  TxAuxScriptsNone       -> Null
  TxAuxScripts _ scripts -> toJSON scripts

friendlyInputs :: [(TxIn, build)] -> Value
friendlyInputs = toJSON . map fst
