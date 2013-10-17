------------------------------------------------------------------------------------------------------------------------

module Main where

import System.Environment(getArgs)
import System.Filesystem(listDir)
import System.Directory(removeFile)
import Data.List(isSuffixOf, isPrefixOf)
import Control.Monad(forM_, filterM)
import Text.Printf(printf)

------------------------------------------------------------------------------------------------------------------------

cppSuffixes :: [String]
cppSuffixes = [".cpp", ".h"]

contentPrefix :: String
contentPrefix = "//automatically generated"

------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   [topDir] <- getArgs
   allFiles <- listDir topDir hasCppSuffix True
   filterFiles <- filterM (fileHasPrefix contentPrefix) allFiles
   forM_ filterFiles $ \file -> do
      _ <- printf "remove file '%s'...\n" file
      removeFile file
   return ()


fileHasPrefix :: String -> FilePath -> IO Bool
fileHasPrefix prefix file = do
   content <- readFile file
   return $! isPrefixOf prefix content 


hasCppSuffix :: FilePath -> Bool
hasCppSuffix fileName = any sFun cppSuffixes
   where
      sFun suffix = suffix `isSuffixOf` fileName


------------------------------------------------------------------------------------------------------------------------
