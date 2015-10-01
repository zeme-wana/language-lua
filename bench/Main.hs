module Main where

import           Criterion.Main
import           Data.Maybe          (catMaybes)
import           System.Directory    (getDirectoryContents)
import           System.FilePath

import qualified Language.Lua.Annotated.Parser  as P

main :: IO ()
main = defaultMain
  [ env (loadFiles "lua-5.3.1-tests") $ \files ->
      bench "Parsing Lua files from 5.3.1 test suite" $
        nf (catMaybes . map (either (const Nothing) Just) . map (P.parseText P.chunk)) files
  ]

loadFiles :: FilePath -> IO [String]
loadFiles root = do
  luaFiles <- map (root </>) . filter ((==) ".lua" . takeExtension) <$> getDirectoryContents root
  mapM readFile luaFiles
