{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Codec.Xlsx.Types.Table where

import Control.DeepSeq (NFData)
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.TH (makeLenses)
import Text.XML
import Text.XML.Cursor

import Codec.Xlsx.Parser.Internal
import Codec.Xlsx.Types.AutoFilter
import Codec.Xlsx.Types.Common
import Codec.Xlsx.Writer.Internal

{- | Tables are ranges of data in the worksheet that have special
 behavior applied which allow users to better sort, analyze, format,
 manage, add, and delete data. Tables and table columns can also be
 referenced through formulas by the spreadsheet application using
 friendly names, making formula calculations that use tables much
 easier to understand and maintain. Tables provide a natural way for
 working with large sets of tabular data.

 NOTE: as @headerRowCount@ property isn't yet supported it's
 supposed that it's library user liability to guarantee that the 1st
 row of 'tblRef' range contains cells with names specified in
 `tblColumns`

 Section 18.5 \"Tables\" (p. 1728)
 Section 18.5.1 \"Tables\" (p. 1729)
 Section 18.5.1.2 "table (Table)" (p. 1730)
-}
data Table = Table
    { tblDisplayName :: Text
    -- ^ A string representing the name of the table. This is the name
    -- that shall be used in formula references, and displayed in the UI
    -- to the spreadsheet user.  This name shall not have any spaces in
    -- it, and it shall be unique amongst all other displayNames and
    -- definedNames in the workbook. The character lengths and
    -- restrictions are the same as for definedNames .
    , tblName :: Maybe Text
    -- ^ A string representing the name of the table that is used to
    -- reference the table programmatically through the spreadsheet
    -- applications object model. This string shall be unique per table
    -- per sheet. It has the same length and character restrictions as
    -- for displayName.  By default this should be the same as the
    -- table's 'tblDisplayName' . This name should also be kept in synch with
    -- the displayName when the displayName is updated in the UI by the
    -- spreadsheet user.
    , tblRef :: CellRef
    -- ^ The range on the relevant sheet that the table occupies
    -- expressed using A1 style referencing.
    , tblColumns :: [TableColumn]
    -- ^ columns of this table, specification requires any table to
    -- include at least 1 column
    , tblAutoFilter :: Maybe AutoFilter
    }
    deriving (Eq, Show, Generic)

instance NFData Table

{- | Single table column

 TODO: styling information

 Section 18.5.1.3 "tableColumn (Table Column)" (p. 1735)
-}
data TableColumn = TableColumn
    { tblcName :: Text
    -- ^ A string representing the unique caption of the table
    -- column. This is what shall be displayed in the header row in the
    -- UI, and is referenced through functions. This name shall be
    -- unique per table.
    }
    deriving (Eq, Show, Generic)

instance NFData TableColumn

makeLenses ''Table

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

instance FromCursor Table where
    fromCursor c = do
        tblDisplayName <- fromAttribute "displayName" c
        tblName <- maybeAttribute "name" c
        tblRef <- fromAttribute "ref" c
        tblAutoFilter <- maybeFromElement (n_ "autoFilter") c
        let tblColumns =
                c
                    $/ element (n_ "tableColumns")
                    &/ element (n_ "tableColumn")
                    >=> fmap TableColumn
                        . fromAttribute "name"
        return Table{..}

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

tableToDocument :: Table -> Int -> Document
tableToDocument tbl i =
    documentFromElement "Table generated by xlsx" $
        tableToElement "table" tbl i

tableToElement :: Name -> Table -> Int -> Element
tableToElement nm Table{..} i = elementList nm attrs subElements
  where
    attrs =
        [ "id" .= i
        , "displayName" .= tblDisplayName
        , "ref" .= tblRef
        ]
            ++ catMaybes
                [ "name" .=? tblName
                ]
    subElements =
        maybeToList (toElement "autoFilter" <$> tblAutoFilter)
            ++ maybeToList
                ( nonEmptyCountedElementList
                    "tableColumns"
                    [ leafElement "tableColumn" ["id" .= i', "name" .= tblcName c]
                    | (i', c) <- zip [(1 :: Int) ..] tblColumns
                    ]
                )
