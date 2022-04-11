-- | A module for converting values to and from database columns.
module Prairie.Sql.Column where

import Data.Proxy
import Chronos
import Data.Void
import Data.Text (Text)
import Data.Text qualified as Text
import Data.ByteString
import Data.Kind
import GHC.TypeLits

data ColumnType
    = ColumnTypeInt
    | ColumnTypeVarChar ColumnSize
    | ColumnTypeBlob ColumnSize
    | ColumnTypeOther Text

data ColumnSize
    = ColumnSizeDefault
    | ColumnSizeSpecified Int

class HasColumnType a where
    toColumnType :: proxy a -> ColumnType

instance HasColumnType Int where
    toColumnType _ = ColumnTypeInt

instance HasColumnType Text where
    toColumnType _ = ColumnTypeVarChar ColumnSizeDefault

instance HasColumnType ByteString where
    toColumnType _ = ColumnTypeVarChar ColumnSizeDefault

data ColumnVal
    = ColumnValInt Int
    | ColumnValVarChar Text
    | ColumnValBlob ByteString
    | ColumnValTime Time
    | ColumnDbSpecific ByteString
    deriving stock Show

-- | A type class for a single-column value.
class ToColumnVal a where
    toColumnVal :: a -> ColumnVal

instance ToColumnVal ColumnVal where
    toColumnVal = id

instance ToColumnVal Int where
    toColumnVal = ColumnValInt

instance ToColumnVal Text where
    toColumnVal = ColumnValVarChar

instance ToColumnVal ByteString where
    toColumnVal = ColumnValBlob

instance ToColumnVal Time where
    toColumnVal = ColumnValTime

class (ToColumnVal a) => FromColumnVal a where
    fromColumnVal :: ColumnVal -> Either FromColumnError a

data FromColumnError = ExpectedButGot Text Text

instance FromColumnVal Int where
    fromColumnVal = \case
        ColumnValInt a ->
            pure a
        other ->
            Left $ ExpectedButGot "Int" (tshow other)

tshow :: Show a => a -> Text
tshow = Text.pack . show

newtype Single a = Single a
    deriving newtype (ToColumnVal)

instance HasColumnType a => HasColumnType (Single a) where
    toColumnType _ = toColumnType (Nothing :: Maybe a)
