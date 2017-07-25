--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)

import           Hakyll

import Posts
import People
import Projects
import Util.Pandoc
import Util.Index

main :: IO ()
main = do
  -- TODO possibly load config from a file?
  pandocMathCompilerFns <- setupPandocMathCompiler $ PandocMathCompilerConfig 1000 ["prftree"]
  let
    pandocMathCompiler = pmcfCompiler pandocMathCompilerFns
    renderPandocMath = pmcfRenderPandoc pandocMathCompilerFns

  hakyll $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/**" $ do
        route   idRoute
        compile compressCssCompiler

    match "location.md" $ do
        route niceRoute
        compile $ do
          let locationCtx =
                constField "location-active" "" `mappend` defaultContext
          pandocMathCompiler
            >>= loadAndApplyTemplate "templates/default.html" locationCtx
            >>= relativizeUrls
            >>= removeIndexHtml

    match "contact.md" $ do
        route niceRoute
        compile $ do
          let contactCtx =
                constField "contact-active" "true" `mappend` defaultContext
          pandocMathCompiler
            >>= loadAndApplyTemplate "templates/default.html" contactCtx
            >>= relativizeUrls
            >>= removeIndexHtml

    projectRules pandocMathCompilerFns

    peopleRules pandocMathCompilerFns

    postRules pandocMathCompilerFns

    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll "posts/**"
            let indexCtx =
                    constField "home-active" ""              `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= renderPandocMath
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= removeIndexHtml

    match "templates/*" $ compile templateBodyCompiler
