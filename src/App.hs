-- Copyright © 2014 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module App
where

import Control.Applicative
import Control.Exception
import Codec.Binary.UTF8.String
import Data.Aeson.TH
import Data.Char
import qualified Data.Map as M
import Network.Socket
import System.IO
import System.Process

data APIRequest
  = CurrentMetadata
  | SongsMatching
    { subpath :: String
    }
  | AddToQueue
    { path :: String
    }
  | SkipThisSong
  deriving (Read, Show)

data APIResponse
  = APIError
    { message :: String
    }
  | WorkedFine
  | NowPlaying
    { filename :: String
    , title :: String
    , artist :: String
    , album :: String
    , remaining :: String
    }
  | SearchResults
    { paths :: [String]
    }
  deriving (Read, Show)

deriveJSON (defaultOptions {sumEncoding = ObjectWithSingleField}) ''APIRequest
deriveJSON (defaultOptions {sumEncoding = ObjectWithSingleField}) ''APIResponse

app :: APIRequest -> IO APIResponse
app req = handle (\e -> return $ APIError $ show (e :: SomeException)) $ do
  sock <- socket AF_UNIX Stream 0
  connect sock $ SockAddrUnix "/srv/radio/run/socket"
  liquidSoap <- socketToHandle sock ReadWriteMode
  hSetNewlineMode liquidSoap $ NewlineMode CRLF CRLF

  resp <- case req of

    CurrentMetadata -> do
      hPutStrLn liquidSoap "request.on_air"
      song_id <- hGetLine liquidSoap
      "END" <- hGetLine liquidSoap

      hPutStrLn liquidSoap $ "request.metadata " ++ song_id
      info <- readMap liquidSoap

      let
	filename = M.findWithDefault "<unknown>" "filename" info
	title = M.findWithDefault "<unknown>" "title" info
	artist = M.findWithDefault "<unknown>" "artist" info
	album = M.findWithDefault "<unknown>" "album" info

      hPutStrLn liquidSoap "icecast.remaining"
      seconds <- read <$> hGetLine liquidSoap
      "END" <- hGetLine liquidSoap

      let
	tmin = floor $ seconds / (60 :: Double) :: Int
	tsec = floor $ seconds - (fromIntegral $ tmin * 60) :: Int
	remaining = show tmin ++ "m" ++ show tsec ++ "s"
      return $ NowPlaying {..}

    SongsMatching {..} -> do
      paths <- lines <$> readProcess "/srv/radio/all-songs.sh" (concat [ ["-ipath", "*" ++ w ++ "*"] | w <- words subpath ]) ""
      return $ SearchResults {..}

    AddToQueue {..} -> do
      hPutStrLn liquidSoap $ "queue.push " ++ path
      song_id <- hGetLine liquidSoap
      "END" <- hGetLine liquidSoap

      hPutStrLn liquidSoap $ "request.metadata " ++ song_id
      info <- readMap liquidSoap

      let
	status = M.findWithDefault "<unknown>" "status" info

      if (status == "ready" || status == "playing")
	then return WorkedFine
	else return $ APIError $ "Invalid track. (status=" ++ status ++ ")"

    SkipThisSong -> do
      hPutStrLn liquidSoap "icecast.skip"
      "Done" <- hGetLine liquidSoap
      "END" <- hGetLine liquidSoap
      return WorkedFine

  hPutStrLn liquidSoap "quit"
  "Bye!" <- hGetLine liquidSoap

  return resp

readMap :: Handle -> IO (M.Map String String)
readMap h = _rm M.empty
  where
    _rm m = do
      l <- hGetLine h
      case l of
	"END" -> return m
	(break (== '=') -> (key,'=':'"':(break (== '"') -> (val,"\"")))) -> _rm $ M.insert key (importString val) m

importString :: String -> String
importString str = decodeString $ unescape str
  where
    unescape "" = ""
    unescape ('\\':cs) = toEnum (octalDigitsToInt $ take 3 cs) : unescape (drop 3 cs)
    unescape (c:cs) = c : unescape cs

    octalDigitsToInt ds = sum $ [ digitToInt d * 8 ^ (i :: Int) | d <- reverse ds | i <- [0..] ]
