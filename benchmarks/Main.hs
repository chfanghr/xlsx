{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.Xlsx
import Codec.Xlsx.Parser.Stream
import Codec.Xlsx.Writer.Stream
import Control.DeepSeq
import Control.Monad (void)
import Criterion.Main
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LB
import Data.Conduit qualified as C
import Data.Conduit.Combinators qualified as C
import Data.Maybe
import Optics (view)

main :: IO ()
main = do
    let filename = "data/testInput.xlsx"
    -- "data/6000.rows.x.26.cols.xlsx"
    bs <- BS.readFile filename
    let bs' = LB.fromStrict bs
        parsed :: Xlsx
        parsed = toXlsxFast bs'
    idx <- fmap (fromMaybe (error "ix not found")) $ runXlsxM filename $ makeIndexFromName "Sample list"
    items <- runXlsxM filename $ collectItems idx
    deepseq (parsed, bs', idx, items) (pure ())
    defaultMain
        [ bgroup
            "readFile"
            [ bench "with xlsx" $ nf toXlsx bs'
            , bench "with xlsx fast" $ nf toXlsxFast bs'
            , bench "with stream (counting)" $ nfIO $ runXlsxM filename $ countRowsInSheet idx
            , bench "with stream (reading)" $ nfIO $ runXlsxM filename $ readSheet idx (pure . rwhnf)
            ]
        , bgroup
            "writeFile"
            [ bench "with xlsx" $ nf (fromXlsx 0) parsed
            , bench "with stream (no sst)" $
                nfIO $
                    C.runConduit $
                        void (writeXlsxWithSharedStrings defaultSettings mempty $ C.yieldMany $ view si_row <$> items)
                            C..| C.fold
            , bench "with stream (sst)" $
                nfIO $
                    C.runConduit $
                        void (writeXlsx defaultSettings $ C.yieldMany $ view si_row <$> items)
                            C..| C.fold
            ]
        ]
