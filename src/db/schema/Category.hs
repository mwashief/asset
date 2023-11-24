{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Category where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int
import Data.Text hiding (filter)
import GHC.Generics
import Rel8
import Prelude hiding (filter, id)

newtype CategoryId = CategoryId {toInt64 :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show, ToJSON, FromJSON)

data Category f = Category
  { categoryId :: Column f CategoryId,
    categoryName :: Column f Text,
    categoryDesc :: Column f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (Category f)

instance ToJSON (Category Result)

instance FromJSON (Category Result)

categorySchema :: TableSchema (Category Name)
categorySchema =
  TableSchema
    { name = "category",
      schema = Nothing,
      columns =
        Category
          { categoryId = "category_id",
            categoryName = "category_name",
            categoryDesc = "category_description"
          }
    }

allCategorys =
  each categorySchema

getCategoryById categoryId_ = do
  category <- each categorySchema
  where_ $ lit categoryId_ ==. categoryId category
  return category

insertCategory name desc =
  Insert
    { into = categorySchema,
      rows = values [Category {categoryId = unsafeDefault, categoryName = lit name, categoryDesc = lit desc}],
      onConflict = Abort,
      returning = NumberOfRowsAffected
    }

deleteCategory categoryId_ =
  Delete
    { from = categorySchema,
      using = pure (),
      deleteWhere = \a b -> categoryId b ==. lit categoryId_,
      returning = NumberOfRowsAffected
    }