{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module BuySell where

import qualified Asset
import qualified AssetDelta
import qualified Category
import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson (FromJSON (parseJSON), ToJSON, decode)
import Data.Int
import Data.Maybe
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Hasql.Session
import qualified PostDeltaBody
import Rel8
import Transaction
import qualified Transaction
import Web.Scotty

data DeltaBody = DeltaBody
  { delta :: Double,
    description :: Maybe Text,
    assetId :: Asset.AssetId,
    categoryId :: Maybe Category.CategoryId
  }
  deriving (Eq, Show, Generic, FromJSON)

data TransactionBody = TransactionBody
  { leftDelta :: DeltaBody,
    rightDelta :: DeltaBody
  }
  deriving (Eq, Show, Generic, FromJSON)

data TimeBody = TimeBody
  { start :: Maybe UTCTime,
    end :: Maybe UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON)

buySellEndpoint url connection = do
  get (capture url) $ do
    now <- liftIO getCurrentTime
    timeBody <- jsonData :: ActionM TimeBody
    Right assets <-
      liftIO $
        select_
          ( AssetDelta.getDeltas
              (fromMaybe (UTCTime (fromGregorian 2018 10 27) (secondsToDiffTime 0)) (start timeBody))
              (fromMaybe now (end timeBody))
          )
    json assets

  get (capture $ url ++ "asset/:id") $ do
    now <- liftIO getCurrentTime
    assetId_ <- captureParam "id" :: ActionM Int64
    timeBody <- jsonData :: ActionM TimeBody
    Right assets <-
      liftIO $
        select_
          ( AssetDelta.getDeltasByAsset
              (lit $ Asset.AssetId assetId_)
              (fromMaybe (UTCTime (fromGregorian 2018 10 27) (secondsToDiffTime 0)) (start timeBody))
              (fromMaybe now (end timeBody))
          )
    json assets

  get (capture $ url ++ "/:id") $ do
    assetDeltaId_ <- captureParam "id" :: ActionM Int64
    Right assetDelta <-
      liftIO $
        select_
          ( AssetDelta.getDeltaById
              (AssetDelta.AssetDeltaId assetDeltaId_)
          )
    json assetDelta

  post (capture $ url ++ "/:id") $ do
    assetDeltaId_ <- captureParam "id" :: ActionM Int64
    let previousAssetDeltaId = AssetDelta.AssetDeltaId assetDeltaId_
    deltaBody <- jsonData :: ActionM PostDeltaBody.PostDeltaBody
    Right assetDeltaList <-
      liftIO $
        select_
          ( AssetDelta.getDeltaById
              (AssetDelta.AssetDeltaId assetDeltaId_)
          )
    let assetDelta = head assetDeltaList
    Right newAssetDeltaIds <-
      liftIO $
        update_ $
          AssetDelta.updateAssetDelta
            previousAssetDeltaId
            (fromMaybe (AssetDelta.date assetDelta) (PostDeltaBody.date deltaBody))
            (fromMaybe (AssetDelta.delta assetDelta) (PostDeltaBody.delta deltaBody))
            (PostDeltaBody.description deltaBody <|> AssetDelta.adDesc assetDelta)
            (fromMaybe (AssetDelta.adAssetId assetDelta) (PostDeltaBody.assetId deltaBody))
            (PostDeltaBody.categoryId deltaBody <|> AssetDelta.adCategoryId assetDelta)
    let newAssetDeltaId = head newAssetDeltaIds
    Right lhs <-
      liftIO $
        update__ $
          updateTransactionLhs previousAssetDeltaId newAssetDeltaId
    Right rhs <-
      liftIO $
        update__ $
          updateTransactionRhs previousAssetDeltaId newAssetDeltaId

    json $ (+) lhs rhs

  put (capture (url ++ "/delta")) $ do
    now <- liftIO getCurrentTime
    req <- jsonData :: ActionM DeltaBody
    Right assetDeltaIds <- liftIO $ insert_ (AssetDelta.insertAssetDelta now (delta req) (description req) (assetId req) (categoryId req))
    json assetDeltaIds

  put (capture (url ++ "/transaction")) $ do
    now <- liftIO getCurrentTime
    req <- jsonData :: ActionM TransactionBody
    Right leftAssetId <- liftIO $ insert_ (AssetDelta.insertAssetDelta now (delta $ leftDelta req) (description $ leftDelta req) (assetId $ leftDelta req) (categoryId $ leftDelta req))
    Right rightAssetId <- liftIO $ insert_ (AssetDelta.insertAssetDelta now (delta $ rightDelta req) (description $ rightDelta req) (assetId $ rightDelta req) (categoryId $ rightDelta req))
    Right tnx <- liftIO $ insert_ (Transaction.insertTransaction (head leftAssetId) (head rightAssetId))
    json tnx
  where
    select_ query = run (statement () (select query)) connection
    delete_ query = run (statement () (Rel8.delete query)) connection
    insert_ query = run (statement () (insert query)) connection
    update_ query = run (statement () (update query)) connection
    update__ query = run (statement () (update query)) connection
