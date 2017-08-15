{-# LANGUAGE OverloadedStrings #-}
module People.Common (
    lookupAuthors
  , lookupAuthor
  ) where

import Hakyll

lookupAuthors :: MonadMetadata m => Item a -> m (Maybe [String])
lookupAuthors item = do
  mAuthors <- getMetadataField (itemIdentifier item) "authors"
  pure $ fmap (fmap trim . splitAll ",") mAuthors

lookupAuthor :: MonadMetadata m => Item a -> m (Maybe String)
lookupAuthor item = do
  mAuthor <- getMetadataField (itemIdentifier item) "author"
  pure $ fmap trim mAuthor
