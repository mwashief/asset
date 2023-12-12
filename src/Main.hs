{-# LANGUAGE PolyKinds #-}

module Main where

import Assets (assetEndpoint)
import BuySell (buySellEndpoint)
import Categories (categoryEndpoint)
import DbConnection (getConnection)
import Hasql.Connection (Connection)
import Web.Scotty (ScottyM, scotty)

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
