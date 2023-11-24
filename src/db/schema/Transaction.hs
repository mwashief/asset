{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Transaction where

import AssetDelta
import Data.Aeson (FromJSON, ToJSON)
import Data.Int
import Data.Text hiding (filter)
import GHC.Generics
import Rel8
import Prelude hiding (filter, id)

data Transaction f = Transaction
  { leftDeltaId :: Column f AssetDeltaId,
    rightDeltaId :: Column f AssetDeltaId
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (Transaction f)

instance ToJSON (Transaction Result)

instance FromJSON (Transaction Result)

transactionSchema :: TableSchema (Transaction Name)
transactionSchema =
  TableSchema
    { name = "transaction",
      schema = Nothing,
      columns =
        Transaction
          { leftDeltaId = "left_delta_id",
            rightDeltaId = "right_delta_id"
          }
    }

allTransactions =
  each transactionSchema

insertTransaction ldi rdi =
  Insert
    { into = transactionSchema,
      rows = values [Transaction {leftDeltaId = lit ldi, rightDeltaId = lit rdi}],
      onConflict = Abort,
      returning = Projection leftDeltaId
    }

deleteTransaction ldi rdi =
  Delete
    { from = transactionSchema,
      using = pure (),
      deleteWhere = \a b -> leftDeltaId b ==. lit ldi &&. rightDeltaId b ==. lit rdi,
      returning = NumberOfRowsAffected
    }
