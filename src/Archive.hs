{-# LANGUAGE OverloadedStrings #-}

module Archive where

import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as TIO
import Text.Pandoc
import Text.Pandoc.Walk (walk, query)
import System.FilePath.Posix (takeFileName)
import Data.UUID (UUID, toString)
import Network.URI.Encode (decodeText)

import NoteFile
import qualified Data.Map.Strict as Map

-- TODO: Reader Monad for notes index?
readNote :: FilePath -> Map.Map FilePath NoteFile -> IO Pandoc
readNote fp notesIndex = do
  contents <- TIO.readFile fp
  f <- runIOorExplode $ readHtml def contents
  pure $ convertLinks notesIndex $ removeLineBreaks f

writeNote :: FilePath -> Pandoc -> IO ()
writeNote fp doc =  runIOorExplode (writeOrg def doc) >>= TIO.writeFile fp

lookupLinkUUID :: FilePath -> Map.Map FilePath NoteFile -> Maybe UUID
lookupLinkUUID fp notesIndex = uuid <$> Map.lookup (takeFileName fp) notesIndex

explore :: Pandoc -> [Text]
explore = query go
  where
    go :: Block -> [Text]
    go (Header _ (id, _, _) _) = [id]
    go _ = []

convertIDs :: Map.Map FilePath NoteFile -> Pandoc -> Pandoc
convertIDs notesIndex = walk go
  where
    go :: Block -> Block
    -- Only first-level headers
    go h@(Header 1 (id, cs, kvs) content) =
      case lookupLinkUUID (unpack $ decodeText id <> ".html") notesIndex of
        Nothing -> h
        (Just u) -> Header 1 (pack (toString u), cs, kvs) content
    go x = x

convertLinks :: Map.Map FilePath NoteFile -> Pandoc -> Pandoc
convertLinks notesIndex = walk go
  where
    go :: Inline -> Inline
    go l@(Link attr content (url, title)) =
      case lookupLinkUUID (unpack $ decodeText url) notesIndex of
        Nothing -> l
        (Just u) -> (Link attr content ("id:" <> pack (toString u), title))
    go x = x

removeLineBreaks :: Pandoc -> Pandoc
removeLineBreaks = walk go
  where
    go :: Inline -> Inline
    go LineBreak = Str ""
    go x = x
