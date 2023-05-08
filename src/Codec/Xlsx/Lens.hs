{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | lenses to access sheets, cells and values of 'Xlsx'
module Codec.Xlsx.Lens (
    ixSheet,
    atSheet,
    ixCell,
    ixCellRC,
    ixCellXY,
    atCell,
    atCellRC,
    atCellXY,
    cellValueAt,
    cellValueAtRC,
    cellValueAtXY,
) where

import Codec.Xlsx.Types
import Data.Function (on)
import Data.List (deleteBy)
import Data.Text (Text)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Optics (
    AffineTraversal',
    At (..),
    Index,
    IxValue,
    Ixed (ix),
    Lens',
    atraversal,
    iso,
    lens,
    non,
    (%),
 )

newtype SheetList = SheetList {unSheetList :: [(Text, Worksheet)]}
    deriving (Eq, Show, Generic)

type instance IxValue (SheetList) = Worksheet
type instance Index (SheetList) = Text

instance Ixed SheetList where
    ix k = atraversal matcher updater
      where
        matcher sl@(SheetList l) = case lookup k l of
            Just v -> Right v
            Nothing -> Left sl

        updater (SheetList l) v = SheetList $ upsert k v l
    {-# INLINE ix #-}

instance At SheetList where
    at k = lens getter setter
      where
        getter = lookup k . unSheetList

        setter (SheetList l) v = case v of
            Nothing ->
                SheetList $
                    maybe l (\v' -> deleteBy ((==) `on` fst) (k, v') l) $
                        lookup k l
            Just v' -> SheetList $ upsert k v' l
    {-# INLINE at #-}

upsert :: (Eq k) => k -> v -> [(k, v)] -> [(k, v)]
upsert k v [] = [(k, v)]
upsert k v ((k1, v1) : r) =
    if k == k1
        then (k, v) : r
        else (k1, v1) : upsert k v r

{- | lens giving access to a worksheet from 'Xlsx' object
 by its name
-}
ixSheet :: Text -> AffineTraversal' Xlsx Worksheet
ixSheet s = xlSheets % lens SheetList const % ix s

-- xlSheets . \f -> fmap unSheetList . ix s f . SheetList

{- | 'Control.Lens.At' variant of 'ixSheet' lens

 /Note:/ if there is no such sheet in this workbook then new sheet will be
 added as the last one to the sheet list
-}
atSheet :: Text -> Lens' Xlsx (Maybe Worksheet)
atSheet s = xlSheets % iso SheetList unSheetList % at s

{- | lens giving access to a cell in some worksheet
 by its position, by default row+column index is used
 so this lens is a synonym of 'ixCellRC'
-}
ixCell :: (RowIndex, ColumnIndex) -> AffineTraversal' Worksheet Cell
ixCell = ixCellRC

-- | lens to access cell in a worksheet
ixCellRC :: (RowIndex, ColumnIndex) -> AffineTraversal' Worksheet Cell
ixCellRC i = wsCells % ix i

{- | lens to access cell in a worksheet using more traditional
 x+y coordinates
-}
ixCellXY :: (ColumnIndex, RowIndex) -> AffineTraversal' Worksheet Cell
ixCellXY i = ixCellRC $ swap i

{- | accessor that can read, write or delete cell in a worksheet
 synonym of 'atCellRC' so uses row+column index
-}
atCell :: (RowIndex, ColumnIndex) -> Lens' Worksheet (Maybe Cell)
atCell = atCellRC

-- | lens to read, write or delete cell in a worksheet
atCellRC :: (RowIndex, ColumnIndex) -> Lens' Worksheet (Maybe Cell)
atCellRC i = wsCells % at i

{- | lens to read, write or delete cell in a worksheet
 using more traditional x+y or row+column index
-}
atCellXY :: (ColumnIndex, RowIndex) -> Lens' Worksheet (Maybe Cell)
atCellXY i = atCellRC $ swap i

{- | lens to read, write or delete cell value in a worksheet
 with row+column coordinates, synonym for 'cellValueRC'
-}
cellValueAt :: (RowIndex, ColumnIndex) -> Lens' Worksheet (Maybe CellValue)
cellValueAt = cellValueAtRC

{- | lens to read, write or delete cell value in a worksheet
 using row+column coordinates of that cell
-}
cellValueAtRC :: (RowIndex, ColumnIndex) -> Lens' Worksheet (Maybe CellValue)
cellValueAtRC i = atCell i % non def % cellValue

{- | lens to read, write or delete cell value in a worksheet
 using traditional x+y coordinates
-}
cellValueAtXY :: (ColumnIndex, RowIndex) -> Lens' Worksheet (Maybe CellValue)
cellValueAtXY i = cellValueAtRC $ swap i
