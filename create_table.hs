#!/usr/bin/env stack
-- stack script --resolver lts-21.17 --package bibtex --package parsec --package table-layout
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import           Data.List          (intercalate, sortBy)
import           Data.Maybe         (fromMaybe)
import           Data.Ord           (Down (..))
import System.IO (hPutStrLn, hPutStr, IOMode(..), withFile)
import           Text.BibTeX.Entry  (T (..))
import           Text.BibTeX.Parse  (file)
import           Text.Parsec.String (parseFromFile)

data ArchiveStatus = ArchiveAvailable | ArchiveInProcess | ArchiveUnavailable deriving (Show)

showStatus :: ArchiveStatus -> String
showStatus ArchiveAvailable = "available"
showStatus ArchiveUnavailable = "unavailable"
showStatus ArchiveInProcess = "inProcess"

readStatus :: String -> ArchiveStatus
readStatus "available"   = ArchiveAvailable
readStatus "unavailable" = ArchiveUnavailable
readStatus "inProcess"   = ArchiveInProcess
readStatus s             = error $ "cannot read " ++ s

data TableRow = TableRow {
    trBibKey           :: String,
    trMaybeFirstAuthor :: Maybe String,
    trMaybeYear        :: Maybe String,
    trMaybeMonth       :: Maybe String,
    trStatusPCA        :: ArchiveStatus,
    trStatusPAA        :: ArchiveStatus,
    trStatusPMA        :: ArchiveStatus
} deriving (Show)

main :: IO ()
main = do
    Right bibEntries <- parseFromFile file "bibliography.bib"
    statusTable <- readStatusTable "publication_list.tsv"
    let rows = do
            (key, (statusPCA, statusPAA, statusPMA)) <- statusTable
            let row = case findBibKey key bibEntries of
                    Nothing -> TableRow key Nothing Nothing Nothing statusPCA statusPAA statusPMA
                    Just bibEntry ->
                        let bibFields = fields bibEntry
                            year = "year" `lookup` bibFields
                            month = "month" `lookup` bibFields
                            author = Nothing
                        in  TableRow key author year month statusPCA statusPAA statusPMA
            return row
    let tableBody = do
            TableRow k a y m s1 s2 s3 <- rows
            let aStr = fromMaybe "n/a" a
                yStr = fromMaybe "n/a" y
                mStr = fromMaybe "n/a" m
                dateStr = yStr ++ if mStr /= "n/a" then "-" ++ mStr else ""
            return [k, aStr, dateStr, showStatus s1, showStatus s2, showStatus s3]
    withFile "publication_table.md" WriteMode $ \h -> do
        hPutStr h "| "
        hPutStr h $ intercalate " | " ["Key", "First Author", "Date", "Status PCA", "Status PAA", "Status PMA"]
        hPutStrLn h " |"
        hPutStrLn h "|---|---|---|---|---|---|"
        forM_ tableBody $ \r -> do
            hPutStr h "| "
            hPutStr h $ intercalate " | " r
            hPutStrLn h "| "
        hPutStrLn h "|---|---|---|---|---|---|"


readStatusTable :: FilePath -> IO [(String, (ArchiveStatus, ArchiveStatus, ArchiveStatus))]
readStatusTable fn = map processLine . tail . lines <$> readFile fn
  where
    processLine line =
        let w = words line
            key = head w
            [s1, s2, s3] = map readStatus . tail $ w
        in  (key, (s1, s2, s3))

findBibKey :: String -> [T] -> Maybe T
findBibKey _ [] = Nothing
findBibKey key (entry:restEntries) = if key == identifier entry then Just entry else findBibKey key restEntries
