{-# LANGUAGE OverloadedStrings #-}
module Talks.Common (
    lookupSlidesBase
  ) where

import Hakyll

lookupSlidesBase :: MonadMetadata m => Identifier -> m (Maybe String)
lookupSlidesBase ident =
  getMetadataField ident "slides-base"
