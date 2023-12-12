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

import AssetDelta (AssetDeltaId)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified GHC.Int
import Rel8
  ( Column,
    Delete (Delete, deleteWhere, from, returning, using),
    Expr,
    Insert (Insert, into, onConflict, returning, rows),
    Name,
    OnConflict (Abort),
    Query,
    Rel8able,
    Result,
    Returning (NumberOfRowsAffected, Projection),
    TableSchema (..),
    Update (Update, from, returning, set, target, updateWhere),
    each,
    lit,
    values,
    (&&.),
    (==.),
  )
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

allTransactions :: Query (Transaction Expr)
allTransactions =
  each transactionSchema

insertTransaction :: AssetDeltaId -> AssetDeltaId -> Insert [AssetDeltaId]
insertTransaction ldi rdi =
  Insert
    { into = transactionSchema,
      rows = values [Transaction {leftDeltaId = lit ldi, rightDeltaId = lit rdi}],
      onConflict = Abort,
      returning = Projection leftDeltaId
    }

deleteTransaction :: AssetDeltaId -> AssetDeltaId -> Delete GHC.Int.Int64
deleteTransaction ldi rdi =
  Delete
    { from = transactionSchema,
      using = pure (),
      deleteWhere = \a b -> leftDeltaId b ==. lit ldi &&. rightDeltaId b ==. lit rdi,
      returning = NumberOfRowsAffected
    }

updateTransactionLhs :: AssetDeltaId -> AssetDeltaId -> Update GHC.Int.Int64
updateTransactionLhs prev new =
  Update
    { target = transactionSchema,
      from = pure (),
      set = \from row ->
        row
          { leftDeltaId = lit new
          },
      updateWhere = \a b -> leftDeltaId b ==. lit prev,
      returning = NumberOfRowsAffected
    }

updateTransactionRhs :: AssetDeltaId -> AssetDeltaId -> Update GHC.Int.Int64
updateTransactionRhs prev new =
  Update
    { target = transactionSchema,
      from = pure (),
      set = \from row ->
        row
          { rightDeltaId = lit new
          },
      updateWhere = \a b -> rightDeltaId b ==. lit prev,
      returning = NumberOfRowsAffected
    }
