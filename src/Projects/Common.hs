{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Projects.Common (
    lookupProject
  ) where

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson

import Hakyll

lookupProject :: MonadMetadata m => Item a -> m (Maybe String)
lookupProject item = do
  metadata <- getMetadata (itemIdentifier item)
  return $ case HashMap.lookup "project" metadata of
    Just (Aeson.String p) ->
      Just . Text.unpack $ p
    _ ->
      Nothing
