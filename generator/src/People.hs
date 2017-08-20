{-# LANGUAGE OverloadedStrings #-}
module People (
    peopleRules
  , module People.Context
  ) where

import Data.Functor.Identity (Identity(..))

import Hakyll

import Posts.Context
import People.Common
import People.Context
import Util.Order
import Util.Nick
import Util.Pandoc
import Util.Index
import Util.Items

getPeoplePosts :: String -> Compiler [Item String]
getPeoplePosts = getItems lookupAuthors (loadAll "posts/**")

getPeopleTalks :: String -> Compiler [Item String]
getPeopleTalks = getItems (fmap (fmap Identity) . lookupAuthor) (loadAll $ "talks/*" .&&. hasVersion "snippets")

peopleRules :: PandocMathCompilerFunctions -> Rules ()
peopleRules pmcf = do
  let
    pandocMathCompiler = pmcfCompiler pmcf

  match "snippets/people/*/page.md" $ do
      route $ setExtension "html"
      compile $ do
        nickCtx <- mkNickCtx "snippets/people/*/page.md"
        let peopleCtx = nickCtx `mappend` defaultContext
        pandocMathCompiler
          >>= loadAndApplyTemplate "templates/people-page-snippet.html" peopleCtx

  match "snippets/people/*/posts.md" $ do
      route $ setExtension "html"
      compile $ do
        nickCtx <- mkNickCtx "snippets/people/*/posts.md"
        let peopleCtx = nickCtx `mappend` defaultContext
        pandocMathCompiler
          >>= loadAndApplyTemplate "templates/people-posts-snippet.html" peopleCtx

  create ["people.html"] $ do
      route niceRoute
      compile $ do
          authors <- orderItems =<< loadAll "snippets/people/*/page.md"
          let authorsCtx =
                  constField "people-active" ""                       `mappend`
                  listField "authors" defaultContext (return authors) `mappend`
                  constField "title" "People"                         `mappend`
                  defaultContext

          makeItem ""
              >>= loadAndApplyTemplate "templates/people.html" authorsCtx
              >>= loadAndApplyTemplate "templates/default.html" authorsCtx
              >>= relativizeUrls
          >>= removeIndexHtml

  match "people/*" $ do
      route niceRoute
      compile $ do
        identifier <- getUnderlying
        let
          mIdent =
            case capture "people/*.*" identifier of
              Just [ident, _] -> Just ident
              _ -> Nothing
        peopleTalks <- maybe (pure []) getPeopleTalks mIdent
        peoplePosts <- maybe (pure []) getPeoplePosts mIdent

        let
          peopleTalkCtx =
            if null peopleTalks then mempty else listField "talks" postCtx (pure peopleTalks)
          peoplePostCtx =
            if null peoplePosts then mempty else listField "posts" postCtx (pure peoplePosts)
          peopleCtx =
            constField "people-active" "" `mappend`
            peopleTalkCtx                 `mappend`
            peoplePostCtx                 `mappend`
            defaultContext

        pandocMathCompiler
          >>= loadAndApplyTemplate "templates/person.html"  peopleCtx
          >>= loadAndApplyTemplate "templates/default.html" peopleCtx
          >>= relativizeUrls
          >>= removeIndexHtml
