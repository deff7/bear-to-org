{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import NoteFile
import Archive

import System.FilePath.Posix

import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  notes <- loadNotesList "/home/deff/Repos/Bear-backup/"
  printNotesList notes
  let notesIndex = notesListToMap notes

  docs <- mapM (\n -> do
                   note <- readNote (absolutePath n) notesIndex
                   pure (absolutePath n, note)) notes
  mapM_ (\(path, doc) -> writeNote ("./out" </> (replaceExtension (takeFileName path) "org")) doc) docs

  -- TODO: what the case with Bear callbacks??? See Read to lead.html
  -- TODO: org-id-file ??
