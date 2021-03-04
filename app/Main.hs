{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.Time
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Forward
import           Development.Shake.FilePath
import           GHC.Generics               (Generic)
import           Slick

import qualified Data.HashMap.Lazy as HML
import qualified Data.Text                  as T
import qualified Data.List                  as L
import qualified Data.Ord                   as ORD

---Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta = SiteMeta 
  { siteAuthor = "Phil Curzon"
  , baseUrl = "https://dormouse.io"
  , siteTitle = "Dormoise.io"
  , githubUser = Just "theinnerlight"
  , githubProject = Just "dormouse"
  }

outputFolder :: FilePath
outputFolder = "docs/"

--Data models-------------------------------------------------------------------

withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ HML.union obj siteMetaObj
  where
    Object siteMetaObj = toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

data SiteMeta = SiteMeta 
  { siteAuthor    :: String
  , baseUrl       :: String -- e.g. https://example.ca
  , siteTitle     :: String
  , githubUser    :: Maybe String
  , githubProject :: Maybe String
  } deriving (Generic, Eq, Ord, Show, ToJSON)

type Tag = String

-- | Data for a page
data Page = Page
  { title       :: String
  , author      :: String
  , content     :: String
  , url         :: String
  , date        :: String
  , tags        :: [Tag]
  , description :: String
  , image       :: Maybe String
  , pages       :: Maybe [Page]
  , isTop       :: Bool
  } deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

-- | Find and build all posts
buildPages :: Action [Page]
buildPages = do
  pPaths <- getDirectoryFiles "." ["site//*.md"]
  forP pPaths buildPage

buildPage :: FilePath -> Action Page
buildPage srcPath = cacheAction ("build1" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  postData <- markdownToHTML . T.pack $ postContent
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
  -- Add additional metadata we've been able to compute
  convert . withSiteMeta . withPostUrl $ postData

writePages :: [Page] -> Action()
writePages pages = do
  _ <- forP pages (writePage pages)
  return ()

writePage :: [Page] -> Page -> Action ()
writePage pages page = do
  template <- compileTemplate' "site/templates/post.html"
  let (page' :: Page) = page { pages = Just pages }
      postUrl = url page'
  writeFile' (outputFolder </> postUrl) . T.unpack . substitute template . withSiteMeta $ toJSON page'

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
  void $ forP filepaths $ \filepath ->
    copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

formatDate :: String -> String
formatDate humanDate = toIsoDate parsedTime
  where
    parsedTime =
      parseTimeOrError True defaultTimeLocale "%b %e, %Y" humanDate :: UTCTime

rfc3339 :: Maybe String
rfc3339 = Just "%H:%M:SZ"

toIsoDate :: UTCTime -> String
toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat rfc3339)

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  allPages <- L.sortOn (\x -> (not $ isTop x, title x))  <$> buildPages
  writePages allPages
  copyStaticFiles

main :: IO ()
main = do
  let shOpts = shakeOptions { shakeVerbosity = Chatty, shakeLintInside = ["\\"]}
  shakeArgsForward shOpts buildRules
