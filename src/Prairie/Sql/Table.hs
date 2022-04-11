module Prairie.Sql.Table where

import Prairie
import Prairie.Sql.Row
import Prairie.Sql.Column
import Prairie
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Functor.Const

data FieldDef = FieldDef
    { fieldDefName :: Text
    , fieldDefType :: RowType
    }

class (HasRowType a) => HasTable a where
    tableName :: proxy a -> Text

    fieldDefs :: proxy a -> NonEmpty FieldDef

instance
    ( Typeable a
    , HasRowType a
    , FieldDict HasRowType a
    , Record a
    , HasAtLeastOneField a
    )
  =>
    HasTable (Entity a)
  where
    tableName a = Text.pack . show $ typeRep a

    fieldDefs _ =
        NonEmpty.fromList $ getConst do
            tabulateRecordA @a \field ->
                withFieldDict @HasRowType field do
                    Const
                        [ FieldDef
                            { fieldDefName =
                                recordFieldLabel field
                            , fieldDefType =
                                toRowType field
                            }
                        ]

defaultFieldDefsRecord
    :: forall a proxy.
    ( Typeable a
    , HasRowType a
    , FieldDict HasRowType a
    , Record a
    , HasAtLeastOneField a
    )
    => proxy a
    -> NonEmpty FieldDef
defaultFieldDefsRecord p =
    defaultFieldDefsRecordWith p (\_ a -> a)

defaultFieldDefsRecordWith
    :: forall proxy rec.
    ( Typeable rec
    , HasRowType rec
    , FieldDict HasRowType rec
    , Record rec
    , HasAtLeastOneField rec
    )
    => proxy rec
    -> (forall a. Field rec a -> FieldDef -> FieldDef)
    -> NonEmpty FieldDef
defaultFieldDefsRecordWith p override =
    NonEmpty.fromList $ getConst do
        tabulateRecordA @rec \field ->
            withFieldDict @HasRowType field do
                Const
                    [ override field (defaultFieldToFieldDef field)
                    ]

defaultFieldToFieldDef
    :: (Record rec, HasRowType a)
    => Field rec a
    -> FieldDef
defaultFieldToFieldDef field =
    FieldDef
        { fieldDefName =
            recordFieldLabel field
        , fieldDefType =
            toRowType field
        }

mkCreateTable :: HasTable a => proxy a -> Text
mkCreateTable p = mconcat
    [ "CREATE TABLE "
    , tableName p
    , " ( "
    , mkFieldList
    , " );"
    ]
  where
    mkFieldList =
        Text.intercalate ", " $ NonEmpty.toList $ fmap mkField (fieldDefs p)
    mkField FieldDef {..} =
        -- if a field has a single row, then it's easy
        case fieldDefType of
            RowType (a :| []) ->
                mconcat
                    [ fieldDefName
                    , " "
                    , renderFieldType a
                    ]
            -- if a field has multiple rows, then we prefix the given
            -- fieldDefName on each thing, and uhh i guess show the constr for
            -- the type??
            RowType (a :| xs) ->
                error "implement me"

    renderFieldType :: ColumnType -> Text
    renderFieldType x = case x of
        ColumnTypeInt ->
            "INT"
        ColumnTypeVarChar size ->
            "VARCHAR" <> renderColumnSize size
        ColumnTypeBlob size ->
            "BLOB" <> renderColumnSize size
        ColumnTypeOther txt ->
            txt

    renderColumnSize :: ColumnSize -> Text
    renderColumnSize = \case
        ColumnSizeDefault ->
            ""
        ColumnSizeSpecified i ->
            "(" <> tshow i <> ")"
