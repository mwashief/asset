{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module DbConnection where

import Hasql.Connection (Connection, ConnectionError, acquire, settings)

getConnection :: IO (Either ConnectionError Connection)
getConnection = acquire $ settings "localhost" 5432 "postgres" "postgres" "postgres"
