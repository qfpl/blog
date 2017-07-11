module Projects.Context (
    projectFieldCtx
  ) where

import Hakyll

import Projects.Common

projectFieldCtx :: Context String
projectFieldCtx =
  field "project" $ \item -> do
    project <- lookupProject item
    let
      f x =
        loadBody .
        fromFilePath .
        concat $
        ["snippets/projects/", x, "/posts.md"]
    maybe (return "") f project
