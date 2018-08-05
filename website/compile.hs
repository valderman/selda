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

title :: Text
title = "Haskell, software development, Japanese and other stuff"

metaDesc :: Text
metaDesc = "Haskell, software development, Japanese and other stuff."

metaAuthor :: Text
metaAuthor = "Anton Ekblad"

heading :: Text
heading = "Anton Ekblad"

underHeading :: Text
underHeading = "Computer Scientist • Software Developer"

toplinks :: [Link]
toplinks = []


-- Implementation details from here on

data Link = Link
  { linkURL :: Text
  , linkText :: Text
  }

(|>) :: Text -> Text -> Link
(|>) = Link

data Page = Page
  { pageFileName :: FilePath
  , pageSubdirectory :: FilePath
  , pageTemplate :: FilePath
  , pageDescription :: Text
  , pageBigLogo :: Bool
  }

allPages :: [Page]
allPages =
  [ Page ""
         "."
         "default"
         "Selda is a monadic SQL library for Haskell. It uses advanced type magic to enable seamless prepared statements, well-scoped, fully general inner queries, automatic in-process caching, and much more."
         True

  , Page "tutorial"
         "."
         "default"
         "Learn how to build database applications with Selda, starting from basics and gradually progressing towards advanced concepts."
         False

  , Page "ch1-example-explained"
         "tutorial"
         "default"
         "Learn how to create a simple database application with Selda."
         False

  , Page "ch2-destructive-operations"
         "tutorial"
         "default"
         "Learn how to update, delete and modify table rows with Selda."
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
        , siteToplinks = toplinks
        , siteTitle = title
        , siteDescription = pageDescription page
        , siteAuthor = metaAuthor
        , siteHeading = heading
        , siteUnderHeading = underHeading
        , siteBaseUrl = baseUrl
        , siteBigLogo = pageBigLogo page
        }
  return (ctx, template)
  where
    maybeIndex "" = "index"
    maybeIndex f  = f

data PageCtx = PageCtx
  { siteContent :: Text
  , siteToplinks :: [Link]
  , siteTitle :: Text
  , siteDescription :: Text
  , siteAuthor :: Text
  , siteHeading :: Text
  , siteUnderHeading :: Text
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
    , "heading" ~> siteHeading ctx
    , "underheading" ~> siteUnderHeading ctx
    , "toplinks" ~> siteToplinks ctx
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
