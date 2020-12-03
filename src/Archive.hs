{-# LANGUAGE OverloadedStrings #-}

module Archive where

import Data.Text (pack, unpack)
import qualified Data.Text.IO as TIO
import Text.Pandoc
import Text.Pandoc.Walk (walk)
import System.FilePath.Posix (takeFileName)
import Data.UUID (UUID, toString)
import Network.URI.Encode (decodeText)

import NoteFile
import qualified Data.Map.Strict as Map

type Index = Map.Map FilePath NoteFile

-- TODO: Reader Monad for notes index?
readNote :: FilePath -> Index -> IO Pandoc
readNote fp notesIndex = do
  contents <- TIO.readFile fp
  f <- runIOorExplode $ readHtml def contents
  pure $ convertIDs notesIndex $ convertLinks notesIndex $ removeLineBreaks f

writeNote :: FilePath -> Pandoc -> IO ()
writeNote fp doc =  runIOorExplode (writeOrg def doc) >>= TIO.writeFile fp

lookupLinkUUID :: FilePath -> Index -> Maybe UUID
lookupLinkUUID fp notesIndex = uuid <$> Map.lookup (takeFileName fp) notesIndex

convertIDs :: Index -> Pandoc -> Pandoc
convertIDs notesIndex = walk go
  where
    go :: Block -> Block
    -- Only first-level headers
    go h@(Header 1 (id, cs, kvs) content) =
      case lookupLinkUUID (unpack $ decodeText id <> ".html") notesIndex of
        Nothing -> h
        (Just u) -> Header 1 (id, cs, ("ID", pack (toString u)) : kvs ) content
    go x = x

-- TODO: bear-note callbacks too
convertLinks :: Index -> Pandoc -> Pandoc
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
