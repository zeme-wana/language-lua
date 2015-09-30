module Main where

import           Control.Applicative
import           Criterion.Main
import           Control.DeepSeq
import           Data.Maybe          (catMaybes)
import           System.Directory    (getDirectoryContents)
import           System.FilePath
import qualified Text.Parsec.Pos

import qualified Language.Lua.Annotated.Parser  as P1
import qualified Language.Lua.Annotated.Parser2 as P2

instance NFData Text.Parsec.Pos.SourcePos where
  rnf x = x `seq` ()

main :: IO ()
main = defaultMain
  [ env (loadFiles "lua-5.3.1-tests") $ \files ->
      bench "Parsing Lua files from 5.3.1 test suite with parsec" $
        nf (catMaybes . map (either (const Nothing) Just) . map (P1.parseText P1.chunk)) files
  , env (loadFiles "lua-5.3.1-tests") $ \files ->
      bench "Parsing Lua files from 5.3.1 test suite with happy" $
        nf (catMaybes . map (either (const Nothing) Just) . map (P2.parseText P2.chunk)) files
  ]

loadFiles :: FilePath -> IO [String]
loadFiles root = do
  luaFiles <- map (root </>) . filter ((==) ".lua" . takeExtension) <$> getDirectoryContents root
  mapM readFile luaFiles
