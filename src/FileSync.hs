
------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Time.Clock(UTCTime)
import System.Directory(getDirectoryContents, doesFileExist, doesDirectoryExist, getModificationTime)
import System.FilePath((</>), takeFileName)
import Control.Monad(filterM)
import Data.List(sortBy)
import Data.Ord(Ordering(..), compare)
import Data.List(intersect, (\\))
import Control.Monad(forM, forM_)
import System.IO(hClose, hFileSize, IOMode(ReadMode), openFile)
import Control.Exception.Base(bracket)

------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   res <- compDirs "D:\\Projects\\NetworkManager\\dev\\NMDBServer" "D:\\Projects\\NetworkManager\\7.1\\NMDBServer"
   forM_ res $ \r -> do
      putStrLn $ printCompResult r
   


data FileInfo = FileInfo
   {
      fiDir :: !FilePath,
      fiModTime :: {-# UNPACK #-} !UTCTime,
      fiFileSize :: !Integer
   }
   deriving(Show, Eq)


data CompResult 
   = BothDirResult
      {
         bdrSrcPath :: !FilePath,
         bdrDestPath :: !FilePath
      }
   | OnlySrcDirResult
      {
         osdrPath :: !FilePath
      }
      
   | OnlyDesDirResult
      {
         oddrPath :: !FilePath
      }
   | BothFileResult
      {
         bfrFileName :: !FilePath,
         bfrSrcFileInfo :: !FileInfo,
         bfrDestFileInfo :: !FileInfo
      }
      deriving(Show)


printCompResult :: CompResult -> String
printCompResult (BothDirResult src dest) = src ++ " <==> " ++ dest
printCompResult (OnlySrcDirResult src) = src ++ " =>"
printCompResult (OnlyDesDirResult dest) = "<= " ++ dest
printCompResult (BothFileResult name _ _) = name


compDirs :: FilePath -> FilePath -> IO [CompResult]
compDirs sDir dDir = do
   srcDirConts <- getDirectoryContents sDir
   destDirConts <- getDirectoryContents dDir
   let 
      propFile file = file /= "." && file /= ".."
      propSrcDirConts = filter propFile srcDirConts
      propDestDirConts = filter propFile destDirConts
      intersectConts = intersect propSrcDirConts propDestDirConts
      onlySrcConts = propSrcDirConts \\ propDestDirConts
      onlyDestConts = propDestDirConts \\ propSrcDirConts
       
   res0 <- forM intersectConts $ \name -> do
      let 
         srcFullName = sDir </> name   
         destFullName = dDir </> name
         
      isSrcFile <- doesFileExist srcFullName
      isSrcDir <- doesDirectoryExist srcFullName
      isDestFile <- doesFileExist destFullName
      isDestDir <- doesDirectoryExist destFullName
      
      case (isSrcFile, isSrcDir, isDestFile, isDestDir) of
         (True, False, True, False) -> do
            srcModTime <- getModificationTime srcFullName
            srcFileSize <- getFileSize srcFullName
            destModTime <- getModificationTime destFullName
            destFileSize <- getFileSize destFullName
            return $ BothFileResult name (FileInfo sDir srcModTime srcFileSize) (FileInfo dDir destModTime destFileSize)
            
         (False, True, False, True) -> return $ BothDirResult srcFullName destFullName
            
      
  
   return res0

  
getFileSize :: FilePath -> IO Integer
getFileSize path = bracket (openFile path ReadMode) hClose hFileSize
   
------------------------------------------------------------------------------------------------------------------------