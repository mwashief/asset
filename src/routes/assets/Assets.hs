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
  ( AssetId (AssetId),
    allAssets,
    deleteAsset,
    getAssetById,
    insertAsset,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.IO.Handle ()
import Hasql.Connection (Connection)
import Hasql.Session (run, statement)
import Rel8 (delete, insert, select)
import Web.Scotty
  ( ActionM,
    capture,
    captureParam,
    delete,
    get,
    json,
    jsonData,
    put,
  )
import qualified Web.Scotty.Internal.Types

data AssetBody = AssetBody
  { name :: Text,
    unit :: Text,
    description :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON)

assetEndpoint :: String -> Connection -> Web.Scotty.Internal.Types.ScottyT IO ()
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
