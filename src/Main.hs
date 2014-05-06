-- Copyright © 2014 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main
where

import Control.Applicative
import Control.Exception
import Codec.Binary.UTF8.String
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import qualified Data.Map as M
import Network.Socket
import System.IO

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
app req = handle (\e -> return $ APIError $ show (e :: IOError)) $ do
  sock <- socket AF_UNIX Stream 0
  connect sock $ SockAddrUnix "/srv/radio/run/socket"
  liquidSoap <- socketToHandle sock ReadWriteMode
  hSetNewlineMode liquidSoap $ NewlineMode CRLF CRLF

  let
    nowPlaying = do
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

  resp <- case req of

    CurrentMetadata -> nowPlaying

    SongsMatching {..} -> fail "not yet supported"

    AddToQueue {..} -> do
      hPutStrLn liquidSoap $ "queue.push " ++  path
      song_id <- hGetLine liquidSoap
      "END" <- hGetLine liquidSoap

      nowPlaying

    SkipThisSong -> do
      hPutStrLn liquidSoap "alsa.skip"
      "Done" <- hGetLine liquidSoap
      "END" <- hGetLine liquidSoap

      nowPlaying

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
