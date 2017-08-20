{-# LANGUAGE OverloadedStrings #-}
module Util.Items (
    getItems
  ) where

import Data.Maybe (catMaybes)

import Hakyll

getItems :: Foldable t => (Item String -> Compiler (Maybe (t String))) -> Compiler [Item String] -> String -> Compiler [Item String]
getItems lookupParent loadItems parent = do
  items <- recentFirst =<< loadItems
  let itemMatches i = do
        mParents <- lookupParent i
        return $
          mParents >>= \parents ->
            if parent `elem` parents
            then Just i
            else Nothing
  matchingItems <- traverse itemMatches items
  pure . catMaybes $ matchingItems
