module Prairie.Sql.Row where

import Data.Vector (Vector, (!?))
import Data.Vector qualified as Vector
import Data.Kind
import Control.Monad.Except
import Control.Monad.State
import Data.Text (Text)
import Prairie.Sql.ColumnType
import Data.Foldable
import Prairie
import Control.Lens

newtype Row = Row { unRow :: Vector ColumnType }

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

chompCurrentColumnRaw :: RowParser ColumnType
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

newtype Single a = Single a
    deriving newtype ToColumnType

class ToRow a where
    toRow :: a -> Row

instance ToColumnType a => ToRow (Single a) where
    toRow a = Row (pure (toColumnType a))

class FromRow a where
    fromRow :: RowParser a

chompCurrentColumn :: FromColumnType a => RowParser a
chompCurrentColumn = do
    columnType <- chompCurrentColumnRaw
    case fromColumnType columnType of
        Left err ->
            throwError $ RowColumnError err
        Right a ->
            pure a


instance FromColumnType a => FromRow (Single a) where
    fromRow = do
        Single <$> chompCurrentColumn

instance (FromRow a, FromRow b) => FromRow (a, b) where
    fromRow = do
        (,) <$> fromRow <*> fromRow

instance (FromRow a, FromRow b) => FromRow (a :& b) where
    fromRow =
        (:&) <$> fromRow <*> fromRow

data a :& b = a :& b

-- ok, let us begin

newtype Entity a = Entity a

unEntity :: Entity a -> a
unEntity (Entity a) = a

instance FieldDict ToColumnType rec => ToRow (Entity rec) where
    toRow (Entity rec) = Row $ Vector.fromList $ map f allFields
      where
          f :: SomeField rec -> ColumnType
          f (SomeField fld) =
              withFieldDict @ToColumnType fld $
                  toColumnType (view (recordFieldLens fld) rec)



infixr 9 :&
