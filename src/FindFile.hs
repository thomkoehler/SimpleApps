------------------------------------------------------------------------------------------------------------------------

module Main where

import Prelude hiding(catch)

import qualified Data.ByteString as B
import Data.ByteString.Char8(pack)
import Control.Monad.Trans.Resource(runResourceT, allocate, release)
import Control.Monad.Trans.Class(lift)
import System.IO(openFile, hClose, IOMode(ReadMode), hIsEOF, hSeek, SeekMode(RelativeSeek))
import Data.ByteString.Search(breakOn)
import Control.Monad(forM_, when)
import  Control.Exception(catch)
import System.Directory(getDirectoryContents, doesDirectoryExist)
import System.FilePath((</>))
import System.Environment(getArgs)
import System.Console.GetOpt
import System.FilePath.Glob

------------------------------------------------------------------------------------------------------------------------

currBufferSize :: Int
currBufferSize = 10240

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
      do
         mt <- matchText
         when (matchPattern && mt) (putStrLn file)
   )
   `catch` 
   (
      putStrLn . show
   )
   
   where
      matchPattern = case optFilePattern options of
         Nothing -> True
         Just pattern -> match pattern file
      
      matchText = case optText options of
         Nothing -> return True
         Just text -> textFindInFile (pack text) currBufferSize file


textFindInFile :: B.ByteString -> Int -> FilePath -> IO Bool 
textFindInFile text bufferSize fileName = runResourceT $ do
   let
      textSize = B.length text
      propBufferSize = ((bufferSize `div` textSize) + 2) * textSize
      
   (fileReleaser, handle) <- allocate (openFile fileName ReadMode) hClose
   res <- lift $ find handle textSize propBufferSize
   release fileReleaser 
   return res
   
      where
         find handle textSize propBufferSize = do
            buffer <- B.hGetSome handle propBufferSize
            let (_, res) = breakOn text buffer
            if B.null res
               then do
                  isEof <- hIsEOF handle
                  if isEof
                     then return False
                     else do
                        hSeek handle RelativeSeek $ toInteger (- textSize)
                        find handle textSize propBufferSize
               else
                  return True
     
      
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