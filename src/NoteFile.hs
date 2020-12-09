module NoteFile where

import System.Directory
import System.FilePath.Posix (takeFileName, takeExtensions, (</>))
import Data.UUID (UUID, toString)
import System.Random (randomIO)

import qualified Data.Map.Strict as Map

data NoteFile = NoteFile
  { absolutePath :: FilePath
  , uuid :: UUID
  } deriving (Show)

printNotesList :: [NoteFile] -> IO ()
printNotesList = mapM_ p
  where
    p (NoteFile { absolutePath = p, uuid = u }) = putStrLn (p ++ " :: " ++ toString u)

hasExtension :: String -> FilePath -> Bool
hasExtension ext fp = takeExtensions fp == ext

loadNotesList :: FilePath -> IO [NoteFile]
loadNotesList path = listDirectory path >>= convert . filter (hasExtension ".html")
  where
    convert = mapM mkNoteFileWithUUID
    mkNoteFileWithUUID fp = do
      u <- randomIO
      pure NoteFile { absolutePath = path </> fp, uuid = u }

notesListToMap :: [NoteFile] -> Map.Map FilePath NoteFile
notesListToMap fs = Map.fromList kvs
  where
    kvs = map (\nf -> (takeFileName $ absolutePath nf, nf)) fs
