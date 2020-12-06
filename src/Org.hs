module Org where

import qualified Data.Map as Map
import NoteFile
import System.FilePath.Posix
import Data.List (intercalate)
import Data.UUID (toString)

notesIndexToOrgIds :: Map.Map FilePath NoteFile -> String
notesIndexToOrgIds = wrap . intercalate " " . map (display . convert) . Map.elems
  where
    convert note = ( takeFileName . dropExtension . absolutePath $ note, toString $ uuid note )
    display (title, uuid) = "(" <> quote title <> " " <> quote uuid <> ")"
    wrap c = "(" <> c <> ")"
    quote c = "\"" <> escape c <> "\""

    escape "" = ""
    escape ('"':xs) = '\\' : '"' : escape xs
    escape (x:xs) = x : escape xs
