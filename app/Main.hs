{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import NoteFile
import Archive

import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  notes <- loadNotesList "./test/data/bear html 2/"
  printNotesList notes
  let notesIndex = notesListToMap notes

  note <- readNote (absolutePath $ notesIndex Map.! "Read to lead.html") notesIndex
  -- TODO: what the case with Bear callbacks??? See Read to lead.html
  -- TODO: convert links to ORG links. See org-roam files
  -- TODO: org-id-file ??
  print $ explore note
  writeNote "test.org" (convertIDs notesIndex note)
