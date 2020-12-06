{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import NoteFile
import Archive
import Org

import System.FilePath.Posix

import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  notes <- loadNotesList "/home/deff/Repos/Bear-backup/"
  printNotesList notes
  let notesIndex = notesListToMap notes
  let orgPath = "/home/deff/org"

  writeFile (orgPath </> ".orgids") (notesIndexToOrgIds notesIndex)

  docs <- mapM (\n -> do
                   note <- readNote (absolutePath n) notesIndex
                   pure (absolutePath n, note)) notes
  mapM_ (\(path, doc) -> writeNote (orgPath </> (replaceExtension (takeFileName path) "org")) doc) docs
