{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant
import Network.Wai.Handler.Warp (run)
import Servant.Server.StaticFiles (serveDirectoryWebApp)
import System.FilePath ((</>))
import Network.Wai.Middleware.Cors



type API = Raw

api :: Proxy API
api = Proxy

server :: Server API
server = serveDirectoryWebApp "static"

app :: Application
app = serve api server


main :: IO ()
main = run 8080 app
