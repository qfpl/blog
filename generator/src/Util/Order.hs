{-# LANGUAGE OverloadedStrings #-}
module Util.Order (
    orderItems
  ) where

import Data.List (sortBy)
import Text.Read (readMaybe)

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Scientific

import Hakyll

comp :: Maybe Int
     -> Maybe Int
     -> Ordering
comp (Just x) (Just y) =
  compare x y
comp Nothing (Just _) =
  GT
comp (Just _) Nothing =
  LT
comp Nothing Nothing =
  EQ

lookupOrder :: MonadMetadata m
            => Item a
            -> m (Maybe Int)
lookupOrder item = do
  metadata <- getMetadata (itemIdentifier item)
  return $ case HashMap.lookup "order" metadata of
    Just (Aeson.Number n) ->
      Scientific.toBoundedInteger n
    Just (Aeson.String p) ->
      readMaybe . Text.unpack $ p
    _ ->
      Nothing

orderItems :: MonadMetadata m
           => [Item a]
           -> m [Item a]
orderItems items = do
  orders <- traverse lookupOrder items
  return .
    fmap fst .
    sortBy (\x y -> comp (snd x) (snd y)) .
    zip items $
    orders
