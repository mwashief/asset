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

import Asset (Asset (assetId), AssetId, assetSchema)
import AssetClass
  ( AssetClass (assetClassId),
    AssetClassId,
    assetClassSchema,
  )
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text ()
import GHC.Generics (Generic)
import Rel8
  ( Column,
    Delete (..),
    Expr,
    Insert (..),
    Name,
    OnConflict (Abort),
    Query,
    Rel8able,
    Result,
    Returning (NumberOfRowsAffected),
    TableSchema (..),
    each,
    lit,
    values,
    where_,
    (&&.),
    (==.),
  )
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

allAssetClassMappings :: Query (AssetClassMapping Expr)
allAssetClassMappings =
  each assetClassMappingSchema

getAssetClassesByAsset :: Expr AssetId -> Query (AssetClass Expr)
getAssetClassesByAsset assetId_ = do
  assetMap <- each assetClassMappingSchema
  where_ $ acmAssetId assetMap ==. assetId_
  assetClass <- each assetClassSchema
  where_ $ assetClassId assetClass ==. acmAssetClassId assetMap
  return assetClass

getAssetsByAssetClass :: AssetClassId -> Query (Asset Expr)
getAssetsByAssetClass assetClassId_ = do
  assetMap <- each assetClassMappingSchema
  where_ $ acmAssetClassId assetMap ==. lit assetClassId_
  asset <- each assetSchema
  where_ $ assetId asset ==. acmAssetId assetMap
  return asset

insertAssetMapping :: AssetId -> AssetClassId -> Insert Int64
insertAssetMapping assetId_ assetClassId_ =
  Insert
    { into = assetClassMappingSchema,
      rows = values [AssetClassMapping {acmAssetClassId = lit assetClassId_, acmAssetId = lit assetId_}],
      onConflict = Abort,
      returning = NumberOfRowsAffected
    }

deleteAsset :: AssetId -> AssetClassId -> Delete Int64
deleteAsset assetId_ assetClassId_ =
  Delete
    { from = assetClassMappingSchema,
      using = pure (),
      deleteWhere = \a b -> (acmAssetId b ==. lit assetId_) &&. (acmAssetClassId b ==. lit assetClassId_),
      returning = NumberOfRowsAffected
    }