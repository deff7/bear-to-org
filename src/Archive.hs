{-# LANGUAGE OverloadedStrings #-}

module Archive where

import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import Data.List (stripPrefix)
import qualified Data.Text.IO as TIO
import Text.Pandoc
import Text.Pandoc.Walk (walk, walkM, query)
import System.FilePath.Posix (takeFileName)
import Data.UUID (UUID, toString)
import Network.URI.Encode (decodeText, decode)

import NoteFile
import qualified Data.Map.Strict as Map

type Index = Map.Map FilePath NoteFile

-- TODO: Reader Monad for notes index?
readNote :: FilePath -> Index -> IO Pandoc
readNote fp notesIndex = do
  contents <- TIO.readFile fp
  f <- runIOorExplode $ readHtml def contents
  pure $ convertIDs notesIndex $ convertLinks notesIndex $ normalizeHeaders $ removeLineBreaks f

writeNote :: FilePath -> Pandoc -> IO ()
writeNote fp doc =  runIOorExplode (writeOrg opts doc) >>= TIO.writeFile fp
  where
    opts = def { writerColumns = 200 }

normalizeHeaders :: Pandoc -> Pandoc
normalizeHeaders p = walk (normalize minLvl) p
  where
    minLvl = minimum $ query go p
    go (Header lvl _ _) = [lvl]
    go _ = []

    normalize minLvl (Header lvl attr cont) = Header (lvl-minLvl+1) attr cont
    normalize _ x = x


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
        (Just u) -> Header 1 ("", cs, ("ID", pack (toString u)) : kvs ) content
    go x = x

convertLinks :: Index -> Pandoc -> Pandoc
convertLinks notesIndex = walk go
  where
    go :: Inline -> Inline
    go l@(Link attr content (url, title)) =
      case lookupLinkUUID (decode $ extractFileName $ unpack url) notesIndex of
        Nothing -> l
        (Just u) -> (Link attr (cleanup content) ("id:" <> pack (toString u), title))
    go x = x

    bearPrefix = "bear://x-callback-url/open-note?title="
    prohibitedChars = ":/" :: String
    extractFileName s =
      case stripPrefix bearPrefix s of
        Nothing -> s
        (Just x) -> filter (\x -> x `notElem` prohibitedChars) x <> ".html"

    cleanup = filter dropLineBreak
      where
        dropLineBreak LineBreak = False
        dropLineBreak _ = True

removeLineBreaks :: Pandoc -> Pandoc
removeLineBreaks = walk go
  where
    go :: Inline -> Inline
    go LineBreak = Str ""
    go x = x
