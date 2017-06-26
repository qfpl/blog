{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
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

postRules :: PandocMathCompilerFunctions -> Rules ()
postRules pmcf = do
  let
    pandocMathCompiler = pmcfCompiler pmcf
  match "posts/*" $ do
      route $ setExtension "html"
      compile $ do

        let
            projectCtx =
              authorFieldCtx `mappend` projectFieldCtx `mappend` postCtx

        pandocMathCompiler
          >>= loadAndApplyTemplate "templates/post.html"    projectCtx
          >>= loadAndApplyTemplate "templates/default.html" projectCtx
          >>= relativizeUrls

  create ["archive.html"] $ do
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
