module Util.Pandoc (
    PandocMathCompilerConfig(..)
  , PandocMathCompilerFunctions(..)
  , setupPandocMathCompiler
  ) where

import qualified Data.Set                  as S
import           Image.LaTeX.Render

import           Text.Pandoc

import           Image.LaTeX.Render.Pandoc

import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Web.Pandoc

import           Hakyll.Contrib.LaTeX

renderPandocWithTransformM :: ReaderOptions
                           -> WriterOptions
                           -> (Pandoc -> Compiler Pandoc)
                           -> Item String
                           -> Compiler (Item String)
renderPandocWithTransformM ropt wopt f item =
    writePandocWith wopt <$> (traverse f =<< readPandocWith ropt item)

formulaOptionsWithPkgs :: [String] -> FormulaOptions -> FormulaOptions
formulaOptionsWithPkgs pkgs fo =
  let
    p = preamble fo
    f :: String -> String
    f x = "\\usepackage{" ++ x ++ "}"
    pkgString = concatMap f pkgs
  in
    fo { preamble = p ++ pkgString}

pandocFormulaOptionsWithPkgs :: [String] -> PandocFormulaOptions
pandocFormulaOptionsWithPkgs pkgs =
  let
    opts = defaultPandocFormulaOptions
    fo = formulaOptionsWithPkgs pkgs . formulaOptions opts
  in
    opts { formulaOptions = fo }

data PandocMathCompilerConfig =
  PandocMathCompilerConfig {
    pmccCacheSize :: Integer
  , pmccPackages  :: [String]
  }

data PandocMathCompilerFunctions =
  PandocMathCompilerFunctions {
    pmcfCompiler     :: Compiler (Item String)
  , pmcfRenderPandoc :: Item String -> Compiler (Item String)
  }

setupPandocMathCompiler :: PandocMathCompilerConfig -> IO PandocMathCompilerFunctions
setupPandocMathCompiler pmcc = do
  renderFormulae <- initFormulaCompilerDataURI (pmccCacheSize pmcc) defaultEnv
  let
    transform =
      renderFormulae .
      pandocFormulaOptionsWithPkgs .
      pmccPackages $
      pmcc

    -- is this the same as getResourceBody >>= renderFn
    compiler =
      pandocCompilerWithTransformM
        defaultHakyllReaderOptions
        writerOptions
        transform
    renderFn =
      renderPandocWithTransformM
        defaultHakyllReaderOptions
        writerOptions
        transform

  return $
    PandocMathCompilerFunctions
      compiler
      renderFn
  where
    writerOptions :: WriterOptions
    writerOptions =
      let
        d = defaultHakyllWriterOptions
      in
        d
        { writerExtensions =
            S.delete Ext_literate_haskell $ writerExtensions d
        }
