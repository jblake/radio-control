-- Copyright © 2014 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Control.Applicative
import Data.Aeson
import Data.Conduit
import Data.Conduit.Attoparsec
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.FastCGI

import App

main :: IO ()
main = run $ \req -> do
  reqOrErr <- requestBody req $$ sinkParser (fromJSON <$> json)
  case reqOrErr of
    Error e -> return $ responseLBS status200 [("Content-type", "application/json")] $ encode $ APIError $ show e
    Success request -> do
      response <- app request
      return $ responseLBS status200 [("Content-type", "application/json")] $ encode response
