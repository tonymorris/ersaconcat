module Data.Aviation.Ersa.Concat where

import Data.Maybe
import Network.HTTP
import Network.URI
import Text.HTML.TagSoup
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import System.Environment
import Prelude

data ErsaDocument =
  ErsaDocument {
    _pg ::
      String
  , _date ::
      String
  , _ver ::
      String
  }
  deriving (Eq, Ord, Show)

hrefs ::
  Tag [Char]
  -> Maybe [Char]
hrefs (TagOpen "a" attrs) =
  case attrs of
    [("href", e)] -> 
      let (s, t) = splitAt 18 e
      in  if s == "/aip/pending/ersa/"
            then
              Just t
            else
              Nothing
    _ ->
      Nothing
hrefs _ =
  Nothing

ersacontentsRequest ::
  ErsaDocument
  -> Request String
ersacontentsRequest (ErsaDocument p d v) =
  Request
    (URI "http:" (Just (URIAuth "" "www.airservicesaustralia.com" "")) "/aip/aip.asp" ("?pg=" ++ p ++ "&vdate=" ++ d ++ "&ver=" ++ v) "")
    GET
    []
    ""

ersacontentsResponse ::
  ErsaDocument
  -> IO String
ersacontentsResponse d =
  do  s <- simpleHTTP (ersacontentsRequest d)
      getResponseBody s

ersahrefs ::
  String
  -> [String]
ersahrefs r =
  parseTags r >>=
    maybeToList . hrefs

ersadocumentsResponse ::
  ErsaDocument
  -> IO [String]
ersadocumentsResponse d =
  ersahrefs <$> ersacontentsResponse d

(>.>) ::
  Monad m =>
  m ExitCode
  -> m ExitCode
  -> m ExitCode
a >.> b =
  do  e <- a
      if e == ExitSuccess
        then
          b
        else
          return e

traverseExitCodes ::
  (Monad m, Foldable t) =>
  (a -> m ExitCode)
  -> t a
  -> m ExitCode
traverseExitCodes f =
  foldr (\a b -> f a >.> b) (return ExitSuccess)

data ErsaConcatDirectories =
  ErsaConcatDirectories {
    _base ::
      ErsaDocument
      -> FilePath
  , _wgetDirectory ::
      FilePath
  , _outDirectory ::
      FilePath
  , _logDirectory ::
      FilePath
  }

defaultErsaConcatDirectories ::
  ErsaDocument
  -> ErsaConcatDirectories
defaultErsaConcatDirectories c =
  let base (ErsaDocument p d v) =
        concat ["ersa_pg-", p, "_vdate-", d, "_ver-", v]
  in  ErsaConcatDirectories
        base
        ("dist" </> base c </> "wget")
        ("dist" </> base c </> "out")
        ("dist" </> base c </> "log")
  
baseuri ::
  String
baseuri =
  "https://www.airservicesaustralia.com/aip/pending/ersa/"

getersadocuments ::
  ErsaConcatDirectories
  -> ErsaDocument
  -> [String]
  -> IO ExitCode
getersadocuments (ErsaConcatDirectories f w _ l) d u =
  do  createDirectoryIfMissing True w
      createDirectoryIfMissing True l
      traverseExitCodes
        (\a ->  rawSystem'
                  (l </> concat [f d, ".get.err"])
                  (l </> concat [f d, ".get.out"])
                  "wget"
                  [
                    "--no-check-certificate"
                  , "-c"
                  , "--show-progress"
                  , "--directory-prefix"
                  , w
                  , baseuri ++ a
                  ])
        u

-- requires pdftk on PATH
concatersadocuments ::
  ErsaConcatDirectories
  -> ErsaDocument
  -> [String]
  -> IO ExitCode
concatersadocuments (ErsaConcatDirectories f w o l) d u =
  do  createDirectoryIfMissing True o
      createDirectoryIfMissing True l
      rawSystem'
        (l </> concat [f d, ".concat.err"])
        (l </> concat [f d, ".concat.out"])
        "pdftk"
        (map (w </>) u ++ ["output", o </> concat [f d, ".pdf"], "verbose"])

ersaconcat ::
  ErsaConcatDirectories
  -> ErsaDocument
  -> IO ExitCode
ersaconcat a d =
  do  r <- ersadocumentsResponse d
      getersadocuments a d r >.>
        concatersadocuments a d r

defaultersaconcat ::
  ErsaDocument
  -> IO ExitCode
defaultersaconcat d =
  ersaconcat (defaultErsaConcatDirectories d) d

rawSystem' ::
  FilePath
  -> FilePath
  -> FilePath
  -> [String]
  -> IO ExitCode
rawSystem' e o cmd args =
  withFile e AppendMode $
    \herr  ->
      withFile o AppendMode $
        \hout ->
          do
            (_, _, _, ph) <-  createProcess_ "ersaconcat" (proc cmd args) {
                                std_out = UseHandle hout
                              , std_err = UseHandle herr
                              }
            waitForProcess ph

main ::
  IO ()
main =
  do  a <- getArgs
      case a of
        p:d:v:_ ->
          defaultersaconcat (ErsaDocument p d v) >>= exitWith
        _ ->
          do  hPutStrLn stderr "Enter three arguments for ERSA <page> <date (dd-Mmm-yyyy where Mmm is the first three letters of the Julian month name)> <version>"
              exitWith (ExitFailure 65535)

