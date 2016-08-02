--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "src/images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "src/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "src/templates/default.html" defaultContext
            >>= relativizeUrls

    match "src/posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "src/templates/post.html"    postCtx
            >>= loadAndApplyTemplate "src/templates/default.html" postCtx
            >>= relativizeUrls

    create ["src/archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "src/templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "src/templates/default.html" archiveCtx
                >>= relativizeUrls


    match "src/index.html" $ do
        route idRoute
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
