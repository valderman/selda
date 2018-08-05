{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where
import Control.Monad
import Data.Aeson (Value (..))
import Data.String
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict, fromStrict)
import qualified Data.Text as Text
import System.Directory
import System.FilePath
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Markdown
import Text.Mustache

import BaseUrl (baseUrl)

tagline :: Text
tagline = "A Haskell SQL Library"

tutorialTagline :: Text
tutorialTagline = "The Tutorial of Selda | " <> tagline

metaAuthor :: Text
metaAuthor = "Anton Ekblad"


-- Implementation details from here on

data Link = Link
  { linkURL :: Text
  , linkText :: Text
  }

(|>) :: Text -> Text -> Link
(|>) = Link

data Page = Page
  { pageTitle :: Text
  , pageFileName :: FilePath
  , pageSubdirectory :: FilePath
  , pageTemplate :: FilePath
  , pageDescription :: Text
  , pageBigLogo :: Bool
  }

allPages :: [Page]
allPages =
  [ Page ("Selda: " <> tagline)
         ""
         "."
         "default"
         "Selda is a monadic SQL library for Haskell. It uses advanced type magic to enable seamless prepared statements, well-scoped, fully general inner queries, automatic in-process caching, and much more."
         True

  , Page ("The Tutorial of Selda | " <> tagline)
         "tutorial"
         "."
         "default"
         "Learn how to build database applications with Selda, starting from basics and gradually progressing towards advanced concepts."
         False

  , Page ("Chapter 1: An Example, Explained | " <> tutorialTagline)
         "ch1-example-explained"
         "tutorial"
         "default"
         "Learn how to create a simple database application with Selda."
         False

  , Page ("Chapter 1: Destructive Operations | " <> tutorialTagline)
         "ch2-destructive-operations"
         "tutorial"
         "default"
         "Learn how to update, delete and modify table rows with Selda."
         False

  , Page ("Chapter 3: Advanced Queries | " <> tutorialTagline)
         "ch3-advanced-queries"
         "tutorial"
         "default"
         "Master database queries using Selda."
         False
  ]

loadPage :: Page -> IO (PageCtx, Template)
loadPage page = do
  let templateFile = "templates" </> pageTemplate page <.> "html"
      pageFile = "pages" </> pageSubdirectory page </> maybeIndex (pageFileName page) <.> "md"
  Right template <- localAutomaticCompile templateFile
  content <- pack <$> readFile pageFile
  let render = toStrict . renderHtml . markdown def . fromStrict
      ctx = PageCtx
        { siteContent = render content
        , siteTitle = pageTitle page
        , siteDescription = pageDescription page
        , siteAuthor = metaAuthor
        , siteBaseUrl = baseUrl
        , siteBigLogo = pageBigLogo page
        }
  return (ctx, template)
  where
    maybeIndex "" = "index"
    maybeIndex f  = f

data PageCtx = PageCtx
  { siteContent :: Text
  , siteTitle :: Text
  , siteDescription :: Text
  , siteAuthor :: Text
  , siteBaseUrl :: Text
  , siteBigLogo :: Bool
  }

instance ToMustache Link where
  toMustache (Link url text) = object ["url" ~> url, "text" ~> text]

instance ToMustache PageCtx where
  toMustache ctx = object
    [ "description" ~> siteDescription ctx
    , "author" ~> siteAuthor ctx
    , "title" ~> siteTitle ctx
    , "content" ~> siteContent ctx
    , "base" ~> siteBaseUrl ctx
    , "biglogo" ~> siteBigLogo ctx
    ]

copyFilesIn :: FilePath -> FilePath -> IO ()
copyFilesIn from to = do
  fs <- getDirectoryContents from
  forM_ [f | f <- fs, take 1 f /= "."] $ \f -> do
    copyFile (from </> f) (to </> f)

writePage :: Page -> IO ()
writePage page = do
  (ctx, template) <- loadPage page
  let content = substitute template ctx
      path = pageSubdirectory page </> pageFileName page
      outFile = siteDirectory </> path </> "index.html"
  createDirectoryIfMissing True (siteDirectory </> path)
  writeFile outFile (unpack content)
  addToSiteMap page

addToSiteMap :: Page -> IO ()
addToSiteMap Page{..} = do
    appendFile sitemapFile (pageUrl <> "\n")
  where
    base = unpack baseUrl
    subdir
      | pageSubdirectory == "." = ""
      | otherwise               = pageSubdirectory
    pageUrl = base </> subdir </> pageFileName
      

sitemapFile :: FilePath
sitemapFile = siteDirectory </> "sitemap.txt"

siteDirectory :: FilePath
siteDirectory = "_site"

main :: IO ()
main = do
  dirExists <- doesDirectoryExist siteDirectory
  when dirExists $ removeDirectoryRecursive siteDirectory
  createDirectoryIfMissing True siteDirectory

  copyFilesIn "assets" siteDirectory
  mapM_ writePage allPages
