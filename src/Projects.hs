{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Projects (
    projectRules
  , module Projects.Context
  ) where

import Data.Maybe (catMaybes)

import Hakyll

import Posts.Context
import Projects.Common
import Projects.Context
import Util.Order
import Util.Nick
import Util.Pandoc

getProjectPosts :: String -> Compiler [Item String]
getProjectPosts project = do
  posts <- recentFirst =<< loadAll "posts/*"
  let postMatches i = do
        p <- lookupProject i
        return $
          if p == Just project
          then Just i
          else Nothing
  matchingPosts <- traverse postMatches posts
  pure . catMaybes $ matchingPosts

projectRules :: PandocMathCompilerFunctions -> Rules ()
projectRules pmcf = do
  let
    pandocMathCompiler = pmcfCompiler pmcf
  match "snippets/projects/*/page.md" $ do
      route $ setExtension "html"
      compile $ do
        nickCtx <- mkNickCtx "snippets/projects/*/page.md"
        let projectCtx = nickCtx `mappend` defaultContext
        pandocMathCompiler
          >>= loadAndApplyTemplate "templates/project-page-snippet.html" projectCtx

  match "snippets/projects/*/posts.md" $ do
      route $ setExtension "html"
      compile $ do
        nickCtx <- mkNickCtx "snippets/projects/*/posts.md"
        let projectCtx = nickCtx `mappend` defaultContext
        pandocMathCompiler
          >>= loadAndApplyTemplate "templates/project-posts-snippet.html" projectCtx

  create ["projects.html"] $ do
      route idRoute
      compile $ do
          projects <- orderItems =<< loadAll "snippets/projects/*/page.md"
          let projectsCtx =
                  constField "projects-active" ""                       `mappend`
                  listField "projects" defaultContext (return projects) `mappend`
                  constField "title" "Projects"                         `mappend`
                  defaultContext

          makeItem ""
              >>= loadAndApplyTemplate "templates/projects.html" projectsCtx
              >>= loadAndApplyTemplate "templates/default.html" projectsCtx
              >>= relativizeUrls

  match "projects/*" $ do
      route $ setExtension "html"
      compile $ do
        identifier <- getUnderlying
        let
          mIdent =
            case capture "projects/*.*" identifier of
              Just [ident, _] -> Just ident
              _ -> Nothing

        let
          projectPostCtx =
            maybe mempty (listField "posts" postCtx . getProjectPosts) mIdent
          projectCtx =
            projectPostCtx `mappend`
            defaultContext

        pandocMathCompiler
          >>= loadAndApplyTemplate "templates/project.html" projectCtx
          >>= loadAndApplyTemplate "templates/default.html" projectCtx
          >>= relativizeUrls
