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

module AssetClass where

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
import Schema (assetManagementSchema)
import Prelude hiding (filter, id)

newtype AssetClassId = AssetClassId {toInt64 :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show, ToJSON, FromJSON)

data AssetClass f = AssetClass
  { assetClassId :: Column f AssetClassId,
    assetClassName :: Column f Text,
    assetClassDesc :: Column f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (AssetClass f)

instance ToJSON (AssetClass Result)

instance FromJSON (AssetClass Result)

assetClassSchema :: TableSchema (AssetClass Name)
assetClassSchema =
  TableSchema
    { name = "assetClass",
      schema = assetManagementSchema,
      columns =
        AssetClass
          { assetClassId = "asset_class_id",
            assetClassName = "asset_class_name",
            assetClassDesc = "asset_class_description"
          }
    }

allAssetClasss :: Query (AssetClass Expr)
allAssetClasss =
  each assetClassSchema

getAssetClassById :: AssetClassId -> Query (AssetClass Expr)
getAssetClassById assetClassId_ = do
  assetClass <- each assetClassSchema
  where_ $ lit assetClassId_ ==. assetClassId assetClass
  return assetClass

insertAssetClass :: Text -> Maybe Text -> Insert Int64
insertAssetClass name desc =
  Insert
    { into = assetClassSchema,
      rows =
        values
          [ AssetClass
              { assetClassId = unsafeDefault,
                assetClassName = lit name,
                assetClassDesc = lit desc
              }
          ],
      onConflict = Abort,
      returning = NumberOfRowsAffected
    }

deleteAssetClass :: AssetClassId -> Delete Int64
deleteAssetClass assetClassId_ =
  Delete
    { from = assetClassSchema,
      using = pure (),
      deleteWhere = \a b -> assetClassId b ==. lit assetClassId_,
      returning = NumberOfRowsAffected
    }
