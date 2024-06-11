
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant
import Network.Wai.Handler.Warp (run)
import Servant.Server.StaticFiles (serveDirectoryFileServer)
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (simpleCors)

type API = Raw

api :: Proxy API
api = Proxy

server :: Server API
server = serveDirectoryFileServer "static"

app :: Application
app = simpleCors $ serve api server

main :: IO ()
main = run 8080 app

