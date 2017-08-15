{-# LANGUAGE OverloadedStrings #-}
module Talks (
    talkRules
  ) where

import Hakyll

import Posts.Context
import People.Context
import Projects.Context
import Util.Pandoc
import Util.Index
import Util.Nick

import Talks.Common

mkSlidesCtx :: Identifier -> Compiler (Context String)
mkSlidesCtx identifier = do
  let
    mIdent =
      case capture "talks/*.*" identifier of
        Just [ident, _] -> Just ident
        _ -> Nothing

  mSlidesBase <- getMetadataField identifier "slides-base"
  let
    mkSlidesUrl i b =
      mconcat ["/talk-contents/", i, "/", b]
    slidesCtx =
      maybe mempty (constField "slides-url") (mkSlidesUrl <$> mIdent <*> mSlidesBase)
  pure slidesCtx

talkRules :: PandocMathCompilerFunctions -> Rules ()
talkRules pmcf = do
  let
    pandocMathCompiler = pmcfCompiler pmcf

  match "talk-contents/**" $ do
    route   idRoute
    compile copyFileCompiler

  match "talks/*" $ version "snippets" $ do
      route niceRoute
      compile $ do
        identifier <- getUnderlying
        slidesCtx <- mkSlidesCtx identifier
        let
          talkCtx =
              slidesCtx `mappend`
              postCtx

        pandocMathCompiler
             >>= loadAndApplyTemplate "templates/talk.html" talkCtx

  match "talks/*" $ do
      route niceRoute
      compile $ do
        identifier <- getUnderlying
        slidesCtx <- mkSlidesCtx identifier
        let
          talkCtx =
              constField "talks-page" "talks-page" `mappend`
              slidesCtx `mappend`
              postCtx

        pandocMathCompiler
             >>= loadAndApplyTemplate "templates/talk.html" talkCtx

  create ["talks.html"] $ do
      route niceRoute
      compile $ do
          talks <- recentFirst =<< loadAll ("talks/*" .&&. hasNoVersion)
          let
            talksCtx =
                  constField "talks-active" ""                    `mappend`
                  listField "talks" defaultContext (return talks) `mappend`
                  constField "title" "Talks"                      `mappend`
                  defaultContext

          makeItem ""
              >>= loadAndApplyTemplate "templates/talks.html" talksCtx
              >>= loadAndApplyTemplate "templates/default.html" talksCtx
              >>= relativizeUrls
              >>= removeIndexHtml

