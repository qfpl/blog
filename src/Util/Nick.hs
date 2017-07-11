{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Util.Nick (
    mkNickCtx
  ) where

import Hakyll

mkNickCtx :: Pattern -> Compiler (Context String)
mkNickCtx x = do
  identifier <- getUnderlying
  let
    mIdent =
      case capture x identifier of
        Just [ident] -> Just ident
        _ -> Nothing
  pure $ maybe mempty (constField "nick") mIdent
