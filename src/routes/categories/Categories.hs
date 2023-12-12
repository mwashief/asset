{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Categories where

import Category
  ( CategoryId (CategoryId),
    allCategorys,
    deleteCategory,
    getCategoryById,
    insertCategory,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON (parseJSON), ToJSON, decode)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
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

data CategoryBody = CategoryBody
  { name :: Text,
    description :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON)

categoryEndpoint :: String -> Connection -> Web.Scotty.Internal.Types.ScottyT IO ()
categoryEndpoint url connection = do
  get (capture url) $ do
    Right categories <- liftIO $ select_ allCategorys
    json categories

  get (capture $ url ++ "/:id") $ do
    categoryId <- captureParam "id" :: ActionM Int64
    Right assets <- liftIO $ select_ (getCategoryById $ CategoryId categoryId)
    json assets

  Web.Scotty.delete (capture $ url ++ "/:id") $ do
    categoryId_ <- captureParam "id" :: ActionM Int64
    Right assets <-
      liftIO $ delete_ (deleteCategory (CategoryId categoryId_))
    json assets

  put (capture url) $ do
    requestBody <- jsonData :: ActionM CategoryBody
    Right assets <-
      liftIO $
        insert_ $
          insertCategory
            (name requestBody)
            (description requestBody)

    json assets
  where
    select_ query = run (statement () (select query)) connection
    delete_ query = run (statement () (Rel8.delete query)) connection
    insert_ query = run (statement () (insert query)) connection