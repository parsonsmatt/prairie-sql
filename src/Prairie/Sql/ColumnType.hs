module Prairie.Sql.ColumnType where

import Chronos
import Data.Void
import Data.Text (Text)
import Data.Text qualified as Text
import Data.ByteString
import Data.Kind

data ColumnType
    = ColumnInt Int
    | ColumnVarChar Text
    | ColumnBlob ByteString
    | ColumnTime Time
    | ColumnDbSpecific ByteString
    deriving stock Show

-- | A type class for a single-column value.
class ToColumnType a where
    toColumnType :: a -> ColumnType

instance ToColumnType ColumnType where
    toColumnType = id

instance ToColumnType Int where
    toColumnType = ColumnInt

instance ToColumnType Text where
    toColumnType = ColumnVarChar

instance ToColumnType ByteString where
    toColumnType = ColumnBlob

instance ToColumnType Time where
    toColumnType = ColumnTime

class (ToColumnType a) => FromColumnType a where
    fromColumnType :: ColumnType -> Either FromColumnError a

data FromColumnError = ExpectedButGot Text Text

instance FromColumnType Int where
    fromColumnType = \case
        ColumnInt a ->
            pure a
        other ->
            Left $ ExpectedButGot "Int" (tshow other)

tshow :: Show a => a -> Text
tshow = Text.pack . show
