{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module PostDeltaBody where

import Asset (AssetId)
import Category (CategoryId)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data PostDeltaBody = DeltaPostBody
  { date :: Maybe UTCTime,
    delta :: Maybe Double,
    description :: Maybe Text,
    assetId :: Maybe Asset.AssetId,
    categoryId :: Maybe Category.CategoryId
  }
  deriving (Eq, Show, Generic, FromJSON)