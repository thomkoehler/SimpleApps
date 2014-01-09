
------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Time.Clock(UTCTime)
import System.Directory(getDirectoryContents, doesFileExist, doesDirectoryExist, getModificationTime)
import System.FilePath((</>), takeFileName, takeDirectory)
import Data.List(intersect, (\\))
import Control.Monad(forM, filterM)
import System.IO(hClose, hFileSize, IOMode(ReadMode), openFile)
import Control.Exception.Base(bracket)
import Data.Sequence(Seq, fromList)
import Data.Foldable(forM_)
import Data.Traversable(forM)
import Control.Monad((>=>))

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   res <- compDirs True "D:\\Projects\\NetworkManager\\dev\\log4cxx" "D:\\Projects\\NetworkManager\\7.1\\log4cxx"
   forM_ res $ \r -> do
      putStrLn $ printCompResult r
   


data FileInfo = FileInfo
   {
      fiDir :: {-# UNPACK #-} !B.ByteString,
      fiModTime :: {-# UNPACK #-} !UTCTime,
      fiFileSize :: !Integer
   }
   deriving(Show, Eq)


data CompResult 
   = BothDirResult
      {
         bdrSrcPath :: {-# UNPACK #-} !B.ByteString,
         bdrDestPath :: {-# UNPACK #-} !B.ByteString
      }
   | OnlySrcDirResult
      {
         osdrPath :: {-# UNPACK #-} !B.ByteString
      }
      
   | OnlyDestDirResult
      {
         oddrPath :: {-# UNPACK #-} !B.ByteString
      }
   | OnlySrcFileResult
      {
         osfrFileName :: {-# UNPACK #-} !B.ByteString,
         osfrFileInfo :: {-# UNPACK #-} !FileInfo
      }
   | OnlyDestFileResult
      {
         odfrFileName :: {-# UNPACK #-} !B.ByteString,
         odfrFileInfo :: {-# UNPACK #-} !FileInfo
      }
   | BothFileResult
      {
         bfrFileName :: {-# UNPACK #-} !B.ByteString,
         bfrSrcFileInfo :: {-# UNPACK #-} !FileInfo,
         bfrDestFileInfo :: {-# UNPACK #-} !FileInfo
      }
      deriving(Show)


printCompResult :: CompResult -> String
printCompResult (BothDirResult src dest) = show src ++ " <==> " ++ show dest
printCompResult (OnlySrcDirResult src) = show src ++ " =>"
printCompResult (OnlyDestDirResult dest) = "<= " ++ show dest
printCompResult (BothFileResult name _ _) = show name


compDirs :: Bool -> FilePath -> FilePath -> IO (Seq CompResult)
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
   
   return $ fromList $ concat $ intersectResults ++ srcResults ++ destResults
   
   where
      getIntersectResults intersectConts = Control.Monad.forM intersectConts $ \name -> do
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
                  [
                     BothFileResult 
                        (C.pack name) 
                        (FileInfo (C.pack sDir) srcModTime srcFileSize) 
                        (FileInfo (C.pack dDir) destModTime destFileSize)
                  ]
               
            (False, True, False, True) -> return [BothDirResult (C.pack srcFullName) (C.pack destFullName)]
            
            (True, False, False, True) -> do
               srcModTime <- getModificationTime srcFullName
               srcFileSize <- getFileSize srcFullName
               return 
                  [
                     OnlySrcFileResult 
                        (C.pack name) 
                        (FileInfo (C.pack sDir) srcModTime srcFileSize), OnlyDestDirResult (C.pack destFullName)
                  ]
               
            (False, True, True, False) -> do
               destModTime <- getModificationTime destFullName
               destFileSize <- getFileSize destFullName
               return 
                  [
                     OnlyDestFileResult 
                        (C.pack name) 
                        (FileInfo (C.pack dDir) destModTime destFileSize), OnlySrcDirResult (C.pack srcFullName)
                  ]
               
            _ -> error "Fatal error: The files system entry can be either a file or a directory."
      
      getOnlyResult genSrc names = Control.Monad.forM names $ \name -> do
         let 
            fullName = sDir </> name
         
         isFile <- doesFileExist fullName
         isDir <- doesDirectoryExist fullName
      
         case (isFile, isDir) of
            (True, False) -> do
               modTime <- getModificationTime fullName
               fileSize <- getFileSize fullName
               if genSrc
                  then return [OnlySrcFileResult (C.pack name) (FileInfo (C.pack sDir) modTime fileSize)]
                  else return [OnlyDestFileResult (C.pack name) (FileInfo (C.pack sDir) modTime fileSize)] 
            
            (False, True) -> if genSrc
               then return [OnlySrcDirResult (C.pack fullName)]
               else return [OnlyDestDirResult (C.pack fullName)]
               
            _ -> error "Fatal error: The files system entry can be either a file or a directory."
      
      extractCompResultDirs (BothDirResult src dest) = [src, dest]
      extractCompResultDirs (OnlySrcDirResult src) = [src]       
      extractCompResultDirs (OnlyDestDirResult dest) = [dest]
      extractCompResultDirs _ = []
      
      extractAllCompResultDirs :: [CompResult] -> [FilePath]
      extractAllCompResultDirs = map C.unpack . concat . map extractCompResultDirs 


getFileSize :: FilePath -> IO Integer
getFileSize path = bracket (openFile path ReadMode) hClose hFileSize
   

createOnlyFileResult :: Bool -> FilePath -> IO CompResult
createOnlyFileResult isSrc file = do
   modTime <- getModificationTime file
   fileSize <- getFileSize file
   let
      fileName = takeFileName file
      dir = takeDirectory file 
   return $ if isSrc
      then OnlySrcFileResult (C.pack fileName) (FileInfo (C.pack dir) modTime fileSize)   
      else OnlyDestFileResult (C.pack fileName) (FileInfo (C.pack dir) modTime fileSize)


compOnlyDir :: Bool -> FilePath -> IO (Seq CompResult)
compOnlyDir isSrc dir = do
   dirConts <- getDirectoryContents dir
   let 
      propFile file = file /= "." && file /= ".."
      propDirConts = map (dir </>) . filter propFile $ dirConts
   
   fileResults <- filterM doesFileExist >=> mapM (createOnlyFileResult isSrc) $ propDirConts 
   dirs <- filterM doesDirectoryExist propDirConts
   Data.Traversable.forM (fromList dirs) $ \dir -> do
      
   
   return $ fromList fileResults    
      
         
   
   
      
  

      

------------------------------------------------------------------------------------------------------------------------