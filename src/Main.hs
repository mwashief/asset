{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import Assets
import BuySell
import Categories
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid ((<>))
import Data.Text.Lazy
import DbConnection
import GHC.Generics
import Hasql.Connection
import Hasql.Session
import Rel8
import Web.Scotty

-- * api

data Point = Point {x :: Int, y :: Int} deriving (Generic, Show)

instance ToJSON Point

instance FromJSON Point

assetManagement :: Connection -> ScottyM ()
assetManagement connection = do
  assetEndpoint "/assets" connection
  categoryEndpoint "/categories" connection
  buySellEndpoint "/buy-sell" connection

main :: IO ()
main = do
  Right connection <- getConnection
  scotty 3000 $ do
    assetManagement connection
