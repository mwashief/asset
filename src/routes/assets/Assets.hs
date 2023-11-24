{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Assets where

import Asset
import Control.Monad.IO.Class
import Data.Aeson
import Data.Int
import Data.Text
import GHC.Generics
import GHC.IO.Handle
import Hasql.Connection
import Hasql.Session
import Rel8 hiding (name)
import Web.Scotty

data AssetBody = AssetBody
  { name :: Text,
    unit :: Text,
    description :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON)

assetEndpoint url connection = do
  get (capture url) $ do
    Right assets <- liftIO $ select_ allAssets
    json assets

  get (capture $ url ++ "/:id") $ do
    assetId <- captureParam "id" :: ActionM Int64
    Right assets <- liftIO $ select_ (getAssetById $ AssetId assetId)
    json assets

  Web.Scotty.delete (capture $ url ++ "/:id") $ do
    assetId_ <- captureParam "id" :: ActionM Int64
    Right assets <-
      liftIO $ delete_ (deleteAsset (AssetId assetId_))

    json assets

  put (capture url) $ do
    requestBody <- jsonData :: ActionM AssetBody
    Right assets <-
      liftIO $
        insert_
          ( insertAsset
              (name requestBody)
              (unit requestBody)
              (description requestBody)
          )

    json assets
  where
    select_ query = run (statement () (select query)) connection
    delete_ query = run (statement () (Rel8.delete query)) connection
    insert_ query = run (statement () (insert query)) connection
