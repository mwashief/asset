{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Asset where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Rel8
  ( Column,
    DBEq,
    DBType,
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
    unsafeDefault,
    values,
    where_,
    (==.),
  )
import Prelude hiding (filter, id)

newtype AssetId = AssetId {toInt64 :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show, ToJSON, FromJSON)

data Asset f = Asset
  { assetId :: Column f AssetId,
    assetName :: Column f Text,
    assetUnit :: Column f Text,
    assetDesc :: Column f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (Asset f)

instance ToJSON (Asset Result)

instance FromJSON (Asset Result)

assetSchema :: TableSchema (Asset Name)
assetSchema =
  TableSchema
    { name = "asset",
      schema = Nothing,
      columns =
        Asset
          { assetId = "asset_id",
            assetName = "asset_name",
            assetUnit = "unit",
            assetDesc = "asset_description"
          }
    }

allAssets :: Query (Asset Expr)
allAssets =
  each assetSchema

getAssetById :: AssetId -> Query (Asset Expr)
getAssetById assetId_ = do
  asset <- each assetSchema
  where_ $ lit assetId_ ==. assetId asset
  return asset

insertAsset :: Text -> Text -> Maybe Text -> Insert Int64
insertAsset name unit desc =
  Insert
    { into = assetSchema,
      rows = values [Asset {assetId = unsafeDefault, assetName = lit name, assetUnit = lit unit, assetDesc = lit desc}],
      onConflict = Abort,
      returning = NumberOfRowsAffected
    }

deleteAsset :: AssetId -> Delete Int64
deleteAsset assetId_ =
  Delete
    { from = assetSchema,
      using = pure (),
      deleteWhere = \a b -> assetId b ==. lit assetId_,
      returning = NumberOfRowsAffected
    }
