module Posts.Context (
    postCtx
  ) where

import Hakyll

lookupExtras :: MonadMetadata m => String -> Item a -> m (Maybe [String])
lookupExtras tag item = do
  mExtras <- getMetadataField (itemIdentifier item) tag
  pure $ fmap (fmap trim . splitAll ",") mExtras

extras :: String -> Context a
extras tag = listFieldWith tag defaultContext $ \item -> do
  extras <- lookupExtras tag item
  pure $ maybe [] (fmap (\l -> Item (fromFilePath l) l)) extras

postCtx :: Context String
postCtx =
    extras "extra-css" `mappend`
    extras "extra-js" `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
