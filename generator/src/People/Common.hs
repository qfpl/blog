{-# LANGUAGE OverloadedStrings #-}
module People.Common (
    lookupAuthors
  ) where

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson

import Hakyll

lookupAuthors :: MonadMetadata m => Item a -> m (Maybe [String])
lookupAuthors item = do
  metadata <- getMetadata (itemIdentifier item)
  return $ case HashMap.lookup "authors" metadata of
    Just (Aeson.String p) ->
      Just . fmap trim . splitAll "," . Text.unpack $ p
    _ ->
      Nothing
