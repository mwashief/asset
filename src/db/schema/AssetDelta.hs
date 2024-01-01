{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module AssetDelta where

import Asset (Asset (assetId), AssetId)
import AssetClass (AssetClassId)
import AssetClassMapping (getAssetsByAssetClass)
import Category (CategoryId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Rel8
  ( Column,
    DBEq,
    DBType,
    Expr,
    Insert (..),
    Name,
    OnConflict (Abort),
    Query,
    Rel8able,
    Result,
    Returning (Projection),
    TableSchema (TableSchema, columns, name, schema),
    Update (..),
    aggregate,
    asc,
    each,
    lit,
    orderBy,
    sum,
    unsafeDefault,
    values,
    where_,
    (&&.),
    (<=.),
    (==.),
    (>=.),
  )
import Schema (assetManagementSchema)

newtype AssetDeltaId = AssetDeltaId {toInt64 :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show, ToJSON, FromJSON)

data AssetDelta f = AssetDelta
  { assetDeltaId :: Column f AssetDeltaId,
    date :: Column f UTCTime,
    delta :: Column f Double,
    adDesc :: Column f (Maybe Text),
    adAssetId :: Column f AssetId,
    adCategoryId :: Column f (Maybe CategoryId)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (AssetDelta f)

instance ToJSON (AssetDelta Result)

instance FromJSON (AssetDelta Result)

assetDeltaSchema :: TableSchema (AssetDelta Name)
assetDeltaSchema =
  TableSchema
    { name = "asset_delta",
      schema = assetManagementSchema,
      columns =
        AssetDelta
          { assetDeltaId = "asset_delta_id",
            date = "date",
            delta = "delta",
            adDesc = "description",
            adAssetId = "asset_id",
            adCategoryId = "category_id"
          }
    }

insertAssetDelta :: UTCTime -> Double -> Maybe Text -> AssetId -> Maybe CategoryId -> Insert [AssetDeltaId]
insertAssetDelta date delta desc assetId_ categoryId_ =
  Insert
    { into = assetDeltaSchema,
      rows = values [AssetDelta {assetDeltaId = unsafeDefault, date = lit date, delta = lit delta, adDesc = lit desc, adAssetId = lit assetId_, adCategoryId = lit categoryId_}],
      onConflict = Abort,
      returning = Projection assetDeltaId
    }

getDeltaById :: AssetDeltaId -> Query (AssetDelta Expr)
getDeltaById assetDeltaId_ = do
  assetDelta <- each assetDeltaSchema
  where_ $ assetDeltaId assetDelta ==. lit assetDeltaId_
  return assetDelta

getDeltasByAsset :: Expr AssetId -> UTCTime -> UTCTime -> Query (AssetDelta Expr)
getDeltasByAsset assetIdExpr startDate endDate = do
  assetDelta <- orderBy (date >$< asc) $ each assetDeltaSchema
  where_ $ adAssetId assetDelta ==. assetIdExpr &&. date assetDelta >=. lit startDate &&. date assetDelta <=. lit endDate
  return assetDelta

getDeltas :: UTCTime -> UTCTime -> Query (AssetDelta Expr)
getDeltas startDate endDate = do
  assetDelta <- orderBy (date >$< asc) $ each assetDeltaSchema
  where_ $ date assetDelta >=. lit startDate &&. date assetDelta <=. lit endDate
  return assetDelta

getSum :: UTCTime -> UTCTime -> Query (Expr Double)
getSum startDate endDate = aggregate do
  assetDelta <- getDeltas startDate endDate
  return $ Rel8.sum (delta assetDelta)

getDeltasByAssetClass :: AssetClassId -> UTCTime -> UTCTime -> Query (AssetDelta Expr)
getDeltasByAssetClass assetClassId_ startDate endDate = do
  asset <- getAssetsByAssetClass assetClassId_
  getDeltasByAsset (assetId asset) startDate endDate

getSumByAsset :: Expr AssetId -> UTCTime -> UTCTime -> Query (Expr Double)
getSumByAsset assetIdExpr startDate endDate = aggregate do
  assetDelta <- getDeltasByAsset assetIdExpr startDate endDate
  return $ Rel8.sum (delta assetDelta)

updateAssetDelta :: AssetDeltaId -> UTCTime -> Double -> Maybe Text -> AssetId -> Maybe CategoryId -> Update [AssetDeltaId]
updateAssetDelta assetDeltaId_ date_ delta_ adDesc_ adAssetId_ adCategoryId_ =
  Update
    { target = assetDeltaSchema,
      from = pure (),
      set = \from row ->
        row
          { assetDeltaId = unsafeDefault,
            date = lit date_,
            delta = lit delta_,
            adDesc = lit adDesc_,
            adAssetId = lit adAssetId_,
            adCategoryId = lit adCategoryId_
          },
      updateWhere = \a b -> assetDeltaId b ==. lit assetDeltaId_,
      returning = Projection assetDeltaId
    }