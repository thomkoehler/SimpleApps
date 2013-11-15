------------------------------------------------------------------------------------------------------------------------

module Main where

import Data.ByteString.Char8(pack)
import Control.Monad(forM_, when)
import Control.Exception(catch, IOException)
import System.Directory(getDirectoryContents, doesDirectoryExist, canonicalizePath)
import System.FilePath((</>))
import System.Environment(getArgs)
import System.FilePath.Posix(takeFileName)
import System.Console.GetOpt
import System.FilePath.Glob

import System.Filesystem(textFindInFile)

------------------------------------------------------------------------------------------------------------------------

currBufferSize :: Int
currBufferSize = 1024 * 1024

------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   args <- getArgs
   options <- getOptions args
   foreachFile (optTopDir options) (fileFun options) (optRecursive options) 
   return ()
   

fileFun :: Options -> FilePath -> IO ()
fileFun options file =
   (
      if matchPattern
         then do
            mt <- matchText
            when mt $ do
               filePath <- canonicalizePath file
               putStrLn filePath
         else
            return ()
   )
   `catch` 
   (
      \e -> putStrLn ("Error: " ++ show (e :: IOException))
   )
   
   where
      matchPattern = case optFilePattern options of
         Nothing -> True
         Just pattern -> match pattern $ takeFileName $ map bsToSl file
      
      matchText = case optText options of
         Nothing -> return True
         Just text -> textFindInFile (pack text) currBufferSize file
         
      bsToSl c 
         | c == '\\' = '/'
         | otherwise = c

      
foreachFile :: FilePath -> (FilePath -> IO ()) -> Bool -> IO ()
foreachFile topdir fun recursive = do
   names <- getDirectoryContents topdir
   let properNames = filter (`notElem` [".", ".."]) names
   forM_ properNames $ \name -> do
      let path = topdir </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory && recursive
         then foreachFile path fun True
         else fun path
      
------------------------------------------------------------------------------------------------------------------------

data Options = Options
   {
      optTopDir :: FilePath,
      optRecursive :: Bool,
      optText :: Maybe String,
      optFilePattern :: Maybe Pattern
   }
   deriving Show


optDescr :: [OptDescr (Options -> IO Options)]
optDescr = 
   [
      Option "r" ["recursive"]  (NoArg (\opt -> return opt { optRecursive = True })) "search recursive",
      Option "t" ["text"] (ReqArg (\arg opt -> return opt { optText = Just arg }) "<text>") "search text",
      Option "p" ["pattern"] (ReqArg (\arg opt -> return opt { optFilePattern = (Just . compile) arg }) "<pattern>") "file pattern"
   ]

defaultOptions :: FilePath -> Options
defaultOptions topDir = Options topDir False Nothing Nothing

header :: String
header = "Usage: FindFile <dir> [OPTION...]"


getOptions :: [String] -> IO Options
getOptions args
   | null args = error $ usageInfo header optDescr
   | otherwise =
   case getOpt RequireOrder optDescr (tail args) of
      (flags, [], []) -> foldl (>>=) (return (defaultOptions (head args))) flags
      (_, nonOpts, []) -> error $ "unrecognized arguments: " ++ unwords nonOpts
      (_, _, msgs) -> error $ concat msgs ++ usageInfo header optDescr

------------------------------------------------------------------------------------------------------------------------