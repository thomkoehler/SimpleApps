
------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Time.Clock(UTCTime)
import System.Directory(getDirectoryContents, doesFileExist, doesDirectoryExist, getModificationTime)
import System.FilePath((</>))
import Data.List(intersect, (\\))
import Control.Monad(forM, forM_, when)
import System.IO(hClose, hFileSize, IOMode(ReadMode), openFile)
import Control.Exception.Base(bracket)

------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   res <- compDirs True "D:\\Projects\\NetworkManager\\dev\\log4cxx" "D:\\Projects\\NetworkManager\\7.1\\log4cxx"
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
      
   | OnlyDestDirResult
      {
         oddrPath :: !FilePath
      }
   | OnlySrcFileResult
      {
         osfrFileName :: !FilePath,
         osfrFileInfo :: !FileInfo
      }
   | OnlyDestFileResult
      {
         odfrFileName :: !FilePath,
         odfrFileInfo :: !FileInfo
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
printCompResult (OnlyDestDirResult dest) = "<= " ++ dest
printCompResult (BothFileResult name _ _) = name


compDirs :: Bool -> FilePath -> FilePath -> IO [CompResult]
compDirs recursive sDir dDir = do
   srcDirConts <- getDirectoryContents sDir
   destDirConts <- getDirectoryContents dDir
   let 
      propFile file = file /= "." && file /= ".."
      propSrcDirConts = filter propFile srcDirConts
      propDestDirConts = filter propFile destDirConts
      intersectConts = intersect propSrcDirConts propDestDirConts
      onlySrcConts = propSrcDirConts \\ propDestDirConts
      onlyDestConts = propDestDirConts \\ propSrcDirConts
       
   intersectResults <- getIntersectResults intersectConts
   srcResults <- getOnlyResult True onlySrcConts       
   destResults <- getOnlyResult False onlyDestConts
   
  
   return $ concat $ intersectResults ++ srcResults ++ destResults
   
   where
      getIntersectResults intersectConts = forM intersectConts $ \name -> do
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
               return $ 
                  [BothFileResult name (FileInfo sDir srcModTime srcFileSize) (FileInfo dDir destModTime destFileSize)]
               
            (False, True, False, True) -> return [BothDirResult srcFullName destFullName]
            
            (True, False, False, True) -> do
               srcModTime <- getModificationTime srcFullName
               srcFileSize <- getFileSize srcFullName
               return [OnlySrcFileResult name (FileInfo sDir srcModTime srcFileSize), OnlyDestDirResult destFullName]
               
            (False, True, True, False) -> do
               destModTime <- getModificationTime destFullName
               destFileSize <- getFileSize destFullName
               return [OnlyDestFileResult name (FileInfo dDir destModTime destFileSize), OnlySrcDirResult srcFullName]
               
            _ -> error "Fatal error: The files system entry can be either a file or a directory."
      
      getOnlyResult genSrc names = forM names $ \name -> do
         let 
            fullName = sDir </> name
         
         isFile <- doesFileExist fullName
         isDir <- doesDirectoryExist fullName
      
         case (isFile, isDir) of
            (True, False) -> do
               modTime <- getModificationTime fullName
               fileSize <- getFileSize fullName
               if genSrc
                  then return [OnlySrcFileResult name (FileInfo sDir modTime fileSize)]
                  else return [OnlyDestFileResult name (FileInfo sDir modTime fileSize)] 
            
            (False, True) -> if genSrc
               then return [OnlySrcDirResult fullName]
               else return [OnlyDestDirResult fullName]
               
            _ -> error "Fatal error: The files system entry can be either a file or a directory."
      
      extractCompResultDirs (BothDirResult src dest) = [src, dest]
      extractCompResultDirs (OnlySrcDirResult src) = [src]       
      extractCompResultDirs (OnlyDestDirResult dest) = [dest]
      extractCompResultDirs _ = []
      
      extractAllCompResultDirs :: [CompResult] -> [FilePath]
      extractAllCompResultDirs = concat . map extractCompResultDirs 
       
      getFileSize :: FilePath -> IO Integer
      getFileSize path = bracket (openFile path ReadMode) hClose hFileSize
   
------------------------------------------------------------------------------------------------------------------------