module NoteFile where

import System.Directory
import System.FilePath.Posix (replaceExtension, takeFileName, takeExtensions, (</>))
import Data.UUID (UUID, toString)
import System.Random (randomIO)

import Data.Time.Format
import Data.Time.Clock
import Data.Time.Clock.POSIX

import qualified Data.Map.Strict as Map

data NoteFile = NoteFile
  { absolutePath :: FilePath
  , uuid :: UUID
  , timestamp :: UTCTime } deriving (Show)

printNotesList :: [NoteFile] -> IO ()
printNotesList = mapM_ p
  where
    p (NoteFile { absolutePath = p, uuid = u }) = putStrLn (p ++ " :: " ++ toString u)

hasExtension :: String -> FilePath -> Bool
hasExtension ext fp = takeExtensions fp == ext

mkFileName :: NoteFile -> FilePath
mkFileName n = (ts ++) $ sanitizeFileName $ (`replaceExtension` "org") $ takeFileName $ absolutePath n
  where
    ts = (++ "-") . formatTime defaultTimeLocale "%Y%m%d%H%M%S" . timestamp $ n
    sanitizeFileName =
      map
        (\x ->
           case x of
             ' ' -> '_'
             _ -> x)


loadNotesList :: FilePath -> IO [NoteFile]
loadNotesList path = listDirectory path >>= convert . filter (hasExtension ".html")
  where
    convert = mapM mkNoteFileWithUUID
    mkNoteFileWithUUID fp = do
      u <- randomIO
      -- TODO: get time from document!!! It has it in metadata
      ts <- getModificationTime (path </> fp)
      pure NoteFile { absolutePath = path </> fp, uuid = u, timestamp = ts }

notesListToMap :: [NoteFile] -> Map.Map FilePath NoteFile
notesListToMap fs = Map.fromList kvs
  where
    kvs = map (\nf -> (takeFileName $ absolutePath nf, nf)) fs
