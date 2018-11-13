--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Data.Void

import GHC.IO.Encoding

import Hakyll
import Hakyll.Core.Routes
import Hakyll.Core.Identifier
import Text.Pandoc
import Text.Megaparsec hiding ( match )
import Text.Megaparsec.Char

--------------------------------------------------------------------------------
main :: IO ()
main = do
    setLocaleEncoding utf8 
    hakyll $ do
      match "src/images/*" $ do
          route   rmSrcRoute
          compile copyFileCompiler
      match "src/css/*" $ do
          route   rmSrcRoute
          compile compressCssCompiler
      match "src/charts/*" $ do
          route rmSrcRoute
          compile copyFileCompiler
      match "src/contact.markdown" $ do
          route   $ setExtension "html" <.> rmSrcRoute
          compile $ pandocMathCompiler
              >>= loadAndApplyTemplate "src/templates/default.html" defaultContext
              >>= relativizeUrls
      match "src/posts/*" $ do
          route $ setExtension "html" <.> rmSrcRoute
          compile $ pandocCompiler
              >>= loadAndApplyTemplate "src/templates/post.html"    postCtx
              >>= loadAndApplyTemplate "src/templates/default.html" postCtx
              >>= relativizeUrls
      create ["archive.html"] $ do
          route rmSrcRoute
          compile $ do
              posts <- recentFirst =<< loadAll "src/posts/*"
              let archiveCtx =
                      listField "posts" postCtx (return posts) `mappend`
                      constField "title" "Archives"            `mappend`
                      defaultContext

              makeItem "" -- this is the empty item w/ type String
                  >>= loadAndApplyTemplate "src/templates/archive.html" archiveCtx
                  >>= loadAndApplyTemplate "src/templates/default.html" archiveCtx
                  >>= relativizeUrls
      create ["macro-list.html"] $ do
          route rmSrcRoute
          compile $ do
              charts <- recentFirst =<< loadAll ("src/charts/*" .&&. complement "src/charts/*")
              let chartsCtx =
                      listField "charts" defaultContext (return charts) `mappend`
                      constField "title" "Macro Charts" `mappend`
                      defaultContext

              makeItem "" -- this is the empty item w/ type String
                  >>= loadAndApplyTemplate "src/templates/macro-list.html" chartsCtx
                  >>= loadAndApplyTemplate "src/templates/default.html" chartsCtx
                  >>= relativizeUrls
      match "src/index.html" $ do
          route rmSrcRoute
          compile $ do
              posts <- recentFirst =<< loadAll "src/posts/*"
              let indexCtx =
                      listField "posts" postCtx (return posts) `mappend`
                      constField "title" "Home"                `mappend`
                      defaultContext

              getResourceBody
                  >>= applyAsTemplate indexCtx
                  >>= loadAndApplyTemplate "src/templates/default.html" indexCtx
                  >>= relativizeUrls

      match "src/templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

rmSrcRoute :: Routes
rmSrcRoute = customRoute removeSrc

srcParser :: Parsec Void String FilePath
srcParser = do
    string "src/"
    anyChar `manyTill` eof

removeSrc :: Identifier -> FilePath
removeSrc i = case runParser srcParser "srcroute" $ toFilePath i of
                    Left _ -> toFilePath i
                    Right s -> s

-- | Convenience function for 'Routes' composition.
(<.>) :: Routes -> Routes -> Routes
a <.> b = a `composeRoutes` b

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler
    = pandocCompilerWith readerOptions writerOptions where
        readerOptions = defaultHakyllReaderOptions
        writerOptions = defaultHakyllWriterOptions
            { writerHTMLMathMethod = MathJax ""
            }
