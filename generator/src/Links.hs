module Links
  ( getCourseLinksCtx
  ) where

import           Control.Monad (filterM)
import           Data.Time     (UTCTime, parseTimeM)

import           Hakyll

import           Projects
import           Util.Time     (auTimeLocale)

getCourseLinksCtx :: UTCTime -> Context String -> Compiler (Context b)
getCourseLinksCtx nowish cx = do
  courseLinks <- filterM (\itemA -> maybe False (> nowish) . (parseTime =<<) <$> getMetadataField (itemIdentifier itemA) "course-start")
    =<< fmap (take 3) . recentFirst
    =<< getProjectLinks "professional-fp-courses"

  pure $ if null courseLinks
    then mempty
    else listField "courselinks" cx (return courseLinks)
  where
    parseTime = parseTimeM True auTimeLocale "%Y-%m-%d"
