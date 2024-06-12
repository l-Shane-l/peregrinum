{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant
import Servant.HTML.Blaze (HTML)
import Network.Wai.Handler.Warp (run)
import Servant.Server.StaticFiles (serveDirectoryFileServer)
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (simpleCors)
import Text.Blaze.Html5 (Html)
import Control.Monad.IO.Class (liftIO)
import Templates (loadProjects, findProject, projectSummaryTemplate, projectDetailsTemplate, Project)
import System.IO (hPutStrLn, stderr)  -- Import the necessary module

-- Define the API type
type API = "summary" :> Capture "project" String :> Get '[HTML] Html
      :<|> "details" :> Capture "project" String :> Get '[HTML] Html
      :<|> Raw

-- Define the API proxy
api :: Proxy API
api = Proxy

-- Server implementation
server :: Server API
server = projectSummaryHandler :<|> projectDetailsHandler :<|> serveDirectoryFileServer "static"

-- Handler for the /summary/:project endpoint
projectSummaryHandler :: String -> Handler Html
projectSummaryHandler projectName = do
  -- liftIO $ hPutStrLn stderr $ "Received request for project summary: " ++ projectName
  projects <- liftIO loadProjects
  -- liftIO $ hPutStrLn stderr $ "Loaded projects: " ++ show projects
  case findProject projectName projects of
    Just project -> do
      -- liftIO $ hPutStrLn stderr $ "Found project: " ++ show project
      return $ projectSummaryTemplate project
    Nothing -> do
      -- liftIO $ hPutStrLn stderr $ "Project not found: " ++ projectName
      throwError err404 { errBody = "Project not found" }

-- Handler for the /details/:project endpoint
projectDetailsHandler :: String -> Handler Html
projectDetailsHandler projectName = do
  -- liftIO $ hPutStrLn stderr $ "Received request for project details: " ++ projectName
  projects <- liftIO loadProjects
  -- liftIO $ hPutStrLn stderr $ "Loaded projects: " ++ show projects
  case findProject projectName projects of
    Just project -> do
      -- liftIO $ hPutStrLn stderr $ "Found project: " ++ show project
      return $ projectDetailsTemplate project
    Nothing -> do
      -- liftIO $ hPutStrLn stderr $ "Project not found: " ++ projectName
      throwError err404 { errBody = "Project not found" }

-- Application setup with CORS middleware
app :: Application
app = simpleCors $ serve api server

-- Main function to run the server
main :: IO ()
main = run 8080 app

