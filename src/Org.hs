module Org where

import qualified Data.Map as Map
import NoteFile
import Archive
import System.FilePath.Posix
import Data.List (intercalate)
import Data.UUID (toString)
import Text.Pandoc

notesIndexToOrgIds ::[(NoteFile, Pandoc)] -> String
notesIndexToOrgIds = wrap . intercalate " " . map (display . convert)
  where
    convert (note, doc) = ( mkFileName note doc, toString $ uuid note )
    display (title, uuid) = "(" <> quote title <> " " <> quote uuid <> ")"
    wrap c = "(" <> c <> ")"
    quote c = "\"" <> escape c <> "\""

    escape "" = ""
    escape ('"':xs) = '\\' : '"' : escape xs
    escape (x:xs) = x : escape xs
