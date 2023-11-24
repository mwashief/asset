{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module AssetClassMapping where

import Asset
import AssetClass
import Data.Aeson (FromJSON, ToJSON)
import Data.Int
import Data.Text hiding (filter)
import GHC.Generics
import Rel8
import Prelude hiding (filter, id)

data AssetClassMapping f = AssetClassMapping
  { acmAssetClassId :: Column f AssetClassId,
    acmAssetId :: Column f AssetId
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (AssetClassMapping f)

instance ToJSON (AssetClassMapping Result)

instance FromJSON (AssetClassMapping Result)

assetClassMappingSchema :: TableSchema (AssetClassMapping Name)
assetClassMappingSchema =
  TableSchema
    { name = "asset_class_mapping",
      schema = Nothing,
      columns =
        AssetClassMapping
          { acmAssetClassId = "asset_class_id",
            acmAssetId = "asset_id"
          }
    }

allAssetClassMappings =
  each assetClassMappingSchema

getAssetClassesByAsset assetId_ = do
  assetMap <- each assetClassMappingSchema
  where_ $ acmAssetId assetMap ==. assetId_
  assetClass <- each assetClassSchema
  where_ $ assetClassId assetClass ==. acmAssetClassId assetMap
  return assetClass

getAssetsByAssetClass assetClassId_ = do
  assetMap <- each assetClassMappingSchema
  where_ $ acmAssetClassId assetMap ==. lit assetClassId_
  asset <- each assetSchema
  where_ $ assetId asset ==. acmAssetId assetMap
  return asset

insertAssetMapping assetId_ assetClassId_ =
  Insert
    { into = assetClassMappingSchema,
      rows = values [AssetClassMapping {acmAssetClassId = lit assetClassId_, acmAssetId = lit assetId_}],
      onConflict = Abort,
      returning = NumberOfRowsAffected
    }

deleteAsset assetId_ assetClassId_ =
  Delete
    { from = assetClassMappingSchema,
      using = pure (),
      deleteWhere = \a b -> (acmAssetId b ==. lit assetId_) &&. (acmAssetClassId b ==. lit assetClassId_),
      returning = NumberOfRowsAffected
    }