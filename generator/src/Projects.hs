{-# LANGUAGE OverloadedStrings #-}
module Projects (
    projectRules
  , module Projects.Context
  ) where

import Data.Functor.Identity (Identity(..))

import Hakyll

import Posts.Context
import Projects.Common
import Projects.Context
import Util.Order
import Util.Nick
import Util.Pandoc
import Util.Index
import Util.Items

getProjectItems :: Compiler [Item String] -> String -> Compiler [Item String]
getProjectItems = getItems (fmap (fmap Identity). lookupProject)

getProjectPosts :: String -> Compiler [Item String]
getProjectPosts = getProjectItems (loadAll "posts/**")

getProjectTalks :: String -> Compiler [Item String]
getProjectTalks = getProjectItems (loadAll $ "talks/*" .&&. hasVersion "snippets")

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
      route niceRoute
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
              >>= removeIndexHtml

  match "projects/*" $ do
      route niceRoute
      compile $ do
        identifier <- getUnderlying
        let
          mIdent =
            case capture "projects/*.*" identifier of
              Just [ident, _] -> Just ident
              _ -> Nothing
        projectTalks <- maybe (pure []) getProjectTalks mIdent
        projectPosts <- maybe (pure []) getProjectPosts mIdent

        let
          projectTalkCtx =
            if null projectTalks then mempty else listField "talks" postCtx (pure projectTalks)
          projectPostCtx =
            if null projectPosts then mempty else listField "posts" postCtx (pure projectPosts)
          projectCtx =
            constField "projects-active" "" `mappend`
            projectTalkCtx                  `mappend`
            projectPostCtx                  `mappend`
            defaultContext

        pandocMathCompiler
          >>= loadAndApplyTemplate "templates/project.html" projectCtx
          >>= loadAndApplyTemplate "templates/default.html" projectCtx
          >>= relativizeUrls
          >>= removeIndexHtml
