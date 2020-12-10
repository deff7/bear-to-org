{-# LANGUAGE OverloadedStrings #-}

module Archive where

import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import Data.List
import qualified Data.Text.IO as TIO
import Text.Pandoc
import Text.Pandoc.Walk (walk, walkM, query)
import Data.UUID (UUID, toString)
import Network.URI.Encode (decodeText, decode)
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Format.ISO8601
import System.FilePath.Posix (replaceExtension, takeFileName, takeExtensions, (</>))

import NoteFile
import qualified Data.Map.Strict as Map

type Index = Map.Map FilePath NoteFile

readNote :: FilePath -> Index -> IO Pandoc
readNote fp notesIndex = do
  contents <- TIO.readFile fp
  f <- runIOorExplode $ readHtml def contents
  pure $ convertIDs notesIndex $ convertLinks notesIndex $ normalizeHeaders $ removeLineBreaks f

writeNote :: FilePath -> Pandoc -> IO ()
writeNote fp doc =  runIOorExplode (writeOrg opts doc) >>= TIO.writeFile fp
  where
    opts = def { writerColumns = 200 }

-- TODO: maybe implement error handling?
takeCreationTime :: Pandoc -> UTCTime
takeCreationTime (Pandoc (Meta m) _) = convert $ m Map.! "created"
  where
    convert (MetaInlines [Str s]) =
      case parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" (unpack s) of
        Just t -> t

-- Iterative process for solving duplicate time conflicts
mkUniqTimeList' :: [UTCTime] -> [UTCTime]
mkUniqTimeList' = go 0
  where
    go 1000 xs = xs
    go n xs = let xs' = mkUniqTimeList xs in
                if length (group $ sort xs') == length xs
                then xs'
                else go (n+1) xs'

mkUniqTimeList :: [UTCTime] -> [UTCTime]
mkUniqTimeList = concatMap spread . group . sort
  where
    spread ts = zipWith (addUTCTime . fromInteger) ([0..] :: [Integer]) ts

mkFileName :: NoteFile -> UTCTime -> FilePath
mkFileName n t = (ts ++) $ sanitizeFileName $ (`replaceExtension` "org") $ takeFileName $ absolutePath n
  where
    ts = formatTime defaultTimeLocale "%Y%m%d%H%M%S" t ++ "-"
    sanitizeFileName =
      map
        (\x ->
           case x of
             ' ' -> '_'
             _ -> x)

notesIndexToOrgIds ::[(NoteFile, UTCTime)] -> String
notesIndexToOrgIds = wrap . intercalate " " . map (display . convert)
  where
    convert (note, ts) = ( mkFileName note ts, toString $ uuid note )
    display (title, uuid) = "(" <> quote title <> " " <> quote uuid <> ")"
    wrap c = "(" <> c <> ")"
    quote c = "\"" <> escape c <> "\""

    escape "" = ""
    escape ('"':xs) = '\\' : '"' : escape xs
    escape (x:xs) = x : escape xs

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

trimFromInfix :: String -> String -> String
trimFromInfix subs s =
  let parts = zip (tails s) (inits s) in
    case dropWhile (not . isPrefixOf subs . fst) parts of
      [] -> s
      (x:_) -> snd x

cutInfixOut :: String -> String -> String
cutInfixOut _ "" = ""
cutInfixOut inf s@(x:xs) =
  if inf `isPrefixOf` s
  then maybe s id $ stripPrefix inf s
  else x : cutInfixOut inf xs

normalizeLink :: String -> String
normalizeLink = appendExt . sanitizeLink . extractFileName . decode
  where
    bearPrefix = "bear://x-callback-url/open-note?title="
    errorCallback = "&x-error=bear://x-callback-url/create"
    sanitizeLink = cutInfixOut "&header=" . trimFromInfix errorCallback

    appendExt s = if ".html" `isSuffixOf` s then s else s <> ".html"

    extractFileName s =
      case stripPrefix bearPrefix s of
        Nothing -> s
        (Just x) -> x


convertLinks :: Index -> Pandoc -> Pandoc
convertLinks notesIndex = walk go
  where
    go :: Inline -> Inline
    go l@(Link attr content (url, title)) =
      case lookupLinkUUID (normalizeLink $ unpack url) notesIndex of
        Nothing -> Link attr (cleanup content) ("BAD:" <> (pack $ normalizeLink $ unpack url), title)
        (Just u) -> (Link attr (cleanup content) ("id:" <> pack (toString u), title))
    go x = x

    cleanup = filter dropLineBreak
      where
        dropLineBreak LineBreak = False
        dropLineBreak _ = True

convertIDs :: Index -> Pandoc -> Pandoc
convertIDs notesIndex = walk go
  where
    go :: Block -> Block
    -- Only first-level headers
    go h@(Header 1 (id, cs, kvs) content) =
      case lookupLinkUUID (cutProhibited . normalizeLink . unpack $ id) notesIndex of
        Nothing -> h
        (Just u) -> Header 1 ("", cs, ("ID", pack (toString u)) : kvs ) content
    go x = x

    prohibitedChars = ":/" :: String
    cutProhibited = filter (\x -> x `notElem` prohibitedChars)

removeLineBreaks :: Pandoc -> Pandoc
removeLineBreaks = walk go
  where
    go :: Inline -> Inline
    go LineBreak = Str ""
    go x = x
