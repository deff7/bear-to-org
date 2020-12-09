{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import NoteFile
import Archive
import Org

import System.FilePath.Posix

import Control.Monad (when)
import Options.Applicative

data Params = Params
  { notesDirectory :: FilePath
  , outputDirectory :: FilePath
  , generateOrgIds :: Bool
  , verbose :: Bool
  }

opts :: ParserInfo Params
opts = info (flags <**> helper) (fullDesc <> progDesc "Convert notes exported from Bear App to org files")
  where
    flags = Params
      <$> strOption (long "in" <> metavar "PATH" <> help "Directory of HTML notes imported from Bear App")
      <*> strOption (long "out" <> metavar "PATH" <> help "Output directory for org files")
      <*> switch (long "ids" <> short 'i' <> help "Whether to generate .orgids file")
      <*> switch (long "verbose" <> short 'v' <> help "Print processing information")

main :: IO ()
main = do
  params <- execParser opts
  notes <- loadNotesList (notesDirectory params)
  when (verbose params) $ printNotesList notes
  let notesIndex = notesListToMap notes
  let orgPath = outputDirectory params


  docs <- mapM (\n -> do
                   doc <- readNote (absolutePath n) notesIndex
                   pure (n, doc)) notes

  when (generateOrgIds params) $ writeFile (orgPath </> ".orgids") (notesIndexToOrgIds docs)

  mapM_ (\(note, doc) -> writeNote (orgPath </> mkFileName note doc) doc) docs
