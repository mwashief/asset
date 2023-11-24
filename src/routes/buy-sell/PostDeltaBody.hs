{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module PostDeltaBody where

import Asset
import Category
import Data.Aeson
import Data.Text
import Data.Time
import GHC.Generics

data PostDeltaBody = DeltaPostBody
  { date :: Maybe UTCTime,
    delta :: Maybe Double,
    description :: Maybe Text,
    assetId :: Maybe Asset.AssetId,
    categoryId :: Maybe Category.CategoryId
  }
  deriving (Eq, Show, Generic, FromJSON)