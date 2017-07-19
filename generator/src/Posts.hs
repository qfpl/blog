{-# LANGUAGE OverloadedStrings #-}
module Posts (
    postRules
  , module Posts.Context
  ) where

import           Data.Monoid (mappend)
import           Hakyll

import Posts.Context
import People.Context
import Projects.Context
import Util.Pandoc
import Util.Index

postRules :: PandocMathCompilerFunctions -> Rules ()
postRules pmcf = do
  let
    pandocMathCompiler = pmcfCompiler pmcf
  match "posts/*" $ do
      route niceRoute
      compile $ do

        let
            projectCtx =
              authorFieldCtx `mappend` projectFieldCtx `mappend` postCtx

        pandocMathCompiler
          >>= loadAndApplyTemplate "templates/post.html"    projectCtx
          >>= loadAndApplyTemplate "templates/default.html" projectCtx
          >>= relativizeUrls
          >>= removeIndexHtml

  create ["archive/index.html"] $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let archiveCtx =
                  constField "archive-active" ""           `mappend`
                  listField "posts" postCtx (return posts) `mappend`
                  constField "title" "Archives"            `mappend`
                  defaultContext

          makeItem ""
              >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
              >>= loadAndApplyTemplate "templates/default.html" archiveCtx
              >>= relativizeUrls
              >>= removeIndexHtml
