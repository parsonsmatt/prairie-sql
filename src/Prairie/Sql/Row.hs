-- | A module for converting values to and from database rows.
module Prairie.Sql.Row where

import Chronos
import Data.Vector (Vector, (!?))
import Data.Vector qualified as Vector
import Data.Kind
import Control.Monad.Except
import Control.Monad.State
import Data.Text (Text)
import Prairie.Sql.Column
import Data.Foldable
import Prairie
import Control.Lens
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty

newtype Row = Row { unRow :: Vector ColumnVal }

newtype RowParser a
    = RowParser
    { unRowParser :: ExceptT RowError (State RowState) a
    }
    deriving newtype (Functor, Applicative, Monad, MonadError RowError)

data RowState
    = RowState
    { rowStateRow :: Row
    , rowStateIndex :: Int
    }

data RowError
    = RowColumnError FromColumnError
    | NoMoreColumns Int Int

chompCurrentColumnRaw :: RowParser ColumnVal
chompCurrentColumnRaw = do
    RowState {..} <- RowParser get
    case unRow rowStateRow !? rowStateIndex of
        Nothing ->
            throwError $ NoMoreColumns rowStateIndex (length (unRow rowStateRow))
        Just a -> do
            RowParser $ modify \rs -> rs { rowStateIndex = rowStateIndex + 1 }
            pure a

runRowParser :: Row -> RowParser a -> (Either RowError a, RowState)
runRowParser row p =
    runState (runExceptT (unRowParser p)) initialRowState
  where
    initialRowState =
        RowState
            { rowStateRow = row
            , rowStateIndex = 0
            }

evalRowParser :: Row -> RowParser a -> Either RowError a
evalRowParser row p =
    evalState (runExceptT (unRowParser p)) initialRowState
  where
    initialRowState =
        RowState
            { rowStateRow = row
            , rowStateIndex = 0
            }

class ToRow a where
    toRow :: a -> Row

instance ToColumnVal a => ToRow (Single a) where
    toRow a = Row (pure (toColumnVal a))

deriving via Single Int instance ToRow Int
deriving via Single Text instance ToRow Text
deriving via Single ByteString instance ToRow ByteString
deriving via Single Time instance ToRow Time

class FromRow a where
    fromRow :: RowParser a

chompCurrentColumn :: FromColumnVal a => RowParser a
chompCurrentColumn = do
    columnType <- chompCurrentColumnRaw
    case fromColumnVal columnType of
        Left err ->
            throwError $ RowColumnError err
        Right a ->
            pure a

instance FromColumnVal a => FromRow (Single a) where
    fromRow = do
        Single <$> chompCurrentColumn

instance (FromRow a, FromRow b) => FromRow (a, b) where
    fromRow = do
        (,) <$> fromRow <*> fromRow

instance (FromRow a, FromRow b) => FromRow (a :& b) where
    fromRow =
        (:&) <$> fromRow <*> fromRow

newtype RowType = RowType (NonEmpty ColumnType)
    deriving newtype Semigroup

class HasRowType a where
    toRowType :: proxy a -> RowType

instance (HasColumnType a) => HasRowType (Single a) where
    toRowType (x :: proxy (Single a)) =
        RowType (pure (toColumnType x))

instance (HasRowType a, HasRowType b) => HasRowType (a, b) where
    toRowType (_ :: proxy (a, b)) =
        toRowType (Nothing @a) <> toRowType (Nothing @b)

instance (HasRowType a, HasRowType b) => HasRowType (a :& b) where
    toRowType (_ :: proxy (a :& b)) =
        toRowType (Nothing @a) <> toRowType (Nothing @b)

instance (FieldDict HasRowType rec, HasAtLeastOneField rec) => HasRowType (Entity rec) where
    toRowType (_ :: proxy (Entity rec)) =
        RowType $ NonEmpty.fromList $ getConst do
            tabulateRecordA @rec \field ->
                withFieldDict @HasRowType field do
                    let RowType a = toRowType field
                     in Const (toList a)

-- | This class serves as a witness that a type has at least one field.
class HasAtLeastOneField rec where
    atLeastOneFieldProof :: NonEmpty (SomeField rec)

data a :& b = a :& b

infixr 9 :&

-- ok, let us begin

newtype Entity a = Entity a

unEntity :: Entity a -> a
unEntity (Entity a) = a

instance FieldDict ToColumnVal rec => ToRow (Entity rec) where
    toRow (Entity rec) = Row $ Vector.fromList $ map f allFields
      where
          f :: SomeField rec -> ColumnVal
          f (SomeField fld) =
              withFieldDict @ToColumnVal fld $
                  toColumnVal (view (recordFieldLens fld) rec)

instance
    FieldDict FromRow rec
  =>
    FromRow (Entity rec)
  where
    fromRow =
        Entity <$> tabulateRecordA \field ->
            withFieldDict @FromRow field fromRow
