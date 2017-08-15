{-# LANGUAGE OverloadedStrings #-}
module Projects.Common (
    lookupProject
  ) where

import Hakyll

lookupProject :: MonadMetadata m => Item a -> m (Maybe String)
lookupProject item =
  getMetadataField (itemIdentifier item) "project"
