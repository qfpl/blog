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
