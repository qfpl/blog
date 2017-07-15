{-# LANGUAGE OverloadedStrings #-}
module People (
    peopleRules
  , module People.Context
  ) where

import Data.Maybe (catMaybes)

import Hakyll

import Posts.Context
import People.Common
import People.Context
import Util.Order
import Util.Nick
import Util.Pandoc

getPeoplePosts :: String -> Compiler [Item String]
getPeoplePosts person = do
  posts <- recentFirst =<< loadAll "posts/*"
  let postMatches i = do
        mAuthors <- lookupAuthors i
        return $
          mAuthors >>= \authors ->
            if person `elem` authors
            then Just i
            else Nothing
  matchingPosts <- traverse postMatches posts
  pure . catMaybes $ matchingPosts

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
      route idRoute
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

  match "people/*" $ do
      route $ setExtension "html"
      compile $ do
        identifier <- getUnderlying
        let
          mIdent =
            case capture "people/*.*" identifier of
              Just [ident, _] -> Just ident
              _ -> Nothing

        let
          peoplePostCtx =
            maybe mempty (listField "posts" postCtx . getPeoplePosts) mIdent
          peopleCtx =
            peoplePostCtx `mappend`
            defaultContext

        pandocMathCompiler
          >>= loadAndApplyTemplate "templates/person.html"  peopleCtx
          >>= loadAndApplyTemplate "templates/default.html" peopleCtx
          >>= relativizeUrls
