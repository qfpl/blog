module Util.Pandoc (
    PandocMathCompilerConfig(..)
  , PandocMathCompilerFunctions(..)
  , setupPandocMathCompiler
  ) where

import qualified Data.Set                  as S
import           Image.LaTeX.Render

import           Text.Pandoc
import           Text.Pandoc.Walk          (walk)

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

bumpHeaderLevels :: Pandoc -> Pandoc
bumpHeaderLevels =
  let
    sectionDiv e =
      Div (mempty, ["section"], mempty) [e]

    f h@(Header l a i)
      | l == 2 || l == 1 = sectionDiv $ Header 3 a i
      | otherwise        = sectionDiv $ Header (l + 1) a i
    f b = b
  in
    walk f

setupPandocMathCompiler :: PandocMathCompilerConfig -> IO PandocMathCompilerFunctions
setupPandocMathCompiler pmcc = do
  renderFormulae <- initFormulaCompilerDataURI (pmccCacheSize pmcc) defaultEnv
  let
    transform =
      renderFormulae .
      pandocFormulaOptionsWithPkgs .
      pmccPackages $ pmcc


    -- is this the same as getResourceBody >>= renderFn
    compiler =
      pandocCompilerWithTransformM
        defaultHakyllReaderOptions
        writerOptions
        (transform . bumpHeaderLevels)

    renderFn =
      renderPandocWithTransformM
        defaultHakyllReaderOptions
        writerOptions
        (transform . bumpHeaderLevels)

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
