{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates where

import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString as H
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import System.IO (hPutStrLn, stderr)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Project = Project
  { name :: String,
    icon :: String,
    summary :: ProjectInfo,
    details :: ProjectInfo
  }
  deriving (Show, Generic)

data ProjectInfo = ProjectInfo
  { title :: String,
    overview :: String,
    additionalInfo :: String,
    imageSrc :: String,
    conclusion :: Maybe String
  }
  deriving (Show, Generic)

instance FromJSON Project

instance FromJSON ProjectInfo

data ProjectsWrapper where
  ProjectsWrapper :: {projects :: [Project]} -> ProjectsWrapper
  deriving (Show, Generic)

instance FromJSON ProjectsWrapper

loadProjects :: IO [Project]
loadProjects = do
  let jsonFilePath = "projects.json"
  -- hPutStrLn stderr $ "Loading projects from: " ++ jsonFilePath
  jsonData <- B.readFile jsonFilePath
  -- hPutStrLn stderr $ "Raw JSON data: " ++ show jsonData
  let projectsWrapper = decode jsonData :: Maybe ProjectsWrapper
  -- hPutStrLn stderr $ "Decoded projectsWrapper: " ++ show projectsWrapper
  return $ maybe [] projects projectsWrapper

findProject :: String -> [Project] -> Maybe Project
findProject projectName projects =
  let matches = filter (\p -> name p == projectName) projects
   in if null matches then Nothing else Just (Prelude.head matches)

projectSummaryTemplate :: Project -> Html
projectSummaryTemplate project = H.div ! A.id (H.toValue ("project-summary-" ++ name project)) ! A.class_ "bg-white shadow-lg rounded-lg p-6" $ do
  H.div ! A.class_ "container mx-auto px-4 flex flex-col md:flex-row items-center text-center md:text-left" $ do
    H.div ! A.class_ "w-full md:w-1/3 lg:w-1/4 flex justify-center mb-8 md:mb-0" $ do
      H.i ! A.class_ (H.toValue (icon project)) $ ""
    H.div ! A.class_ "w-full md:w-2/3 lg:w-3/4" $ do
      H.h2 ! A.class_ "text-xl font-semibold" $ H.toHtml (title $ summary project)
      H.p ! A.class_ "text-gray-600" $ H.toHtml (overview $ summary project)
    H.div ! A.class_ "mt-4 cursor-pointer flex justify-center md:justify-start" $ do
      H.div
        ! A.class_ "mt-4 cursor-pointer flex flex-col items-center"
        ! H.customAttribute "data-hx-get" (H.toValue $ "/details/" <> H.toValue (title $ summary project))
        ! H.customAttribute "data-hx-target" (H.toValue ("#project-summary-" ++ name project))
        ! H.customAttribute "data-hx-trigger" "click"
        ! H.customAttribute "data-hx-swap" "outerHTML"
        $ do
          H.i ! A.class_ "fas fa-chevron-down text-gray-600 bounce" $ ""

projectDetailsTemplate :: Project -> Html
projectDetailsTemplate project = H.div ! A.id (H.toValue (name project ++ "-details")) ! A.class_ "bg-white shadow-lg rounded-lg p-6 relative" $ do
  H.div ! A.class_ "flex items-center" $ do
    H.i ! A.class_ (H.toValue (icon project ++ " text-blue-800 text-2xl mr-2")) $ ""
    H.h2 ! A.class_ "text-xl font-semibold" $ H.toHtml (title $ details project)
  H.div ! A.class_ "flex flex-wrap mt-4" $ do
    H.div ! A.class_ "w-full md:w-1/2" $ do
      H.img ! A.src (H.toValue (imageSrc $ details project)) ! A.alt (H.toValue (name project ++ " project in action")) ! A.class_ "w-full h-auto"
    H.div ! A.class_ "w-full md:w-1/2 px-4" $ do
      H.h3 ! A.class_ "text-lg font-bold" $ "Project Overview"
      H.p $ H.toHtml (overview $ details project)
      H.br
      H.p $ H.toHtml (additionalInfo $ details project)
      H.br
      maybe (return ()) (H.p . H.toHtml) (conclusion $ details project)
  H.div
    ! A.class_ "mt-4 cursor-pointer flex flex-col items-center"
    ! H.customAttribute "data-hx-get" (H.toValue $ "/summary/" <> H.toValue (title $ summary project))
    ! H.customAttribute "data-hx-target" (H.toValue ("#" ++ name project ++ "-details"))
    ! H.customAttribute "data-hx-trigger" "click"
    ! H.customAttribute "data-hx-swap" "outerHTML"
    $ do
      H.i ! A.class_ "fas fa-chevron-up text-gray-600 bounce" $ ""
