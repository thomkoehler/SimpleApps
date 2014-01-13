
------------------------------------------------------------------------------------------------------------------------

module Main where

import System.Directory(getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath((</>), takeFileName)
import Data.List(intersect, (\\))
import Control.Monad(forM_, filterM, when)
import Text.Printf(printf)

------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = compDirsIter True "./Test/Dir0" "./Test/Dir1" $ \res -> do
   putStrLn $ printResult res
   
   
data Result 
   = DirResult
      {
         drSrcDir :: FilePath,
         drDestDir :: FilePath
      }
   | SrcDirResult
      {
         sdrDir :: FilePath
      }
   | DestDirResult
      {
         ddrDir :: FilePath
      }
   | FileResult
      {
         frSrcFile :: FilePath,
         frDestFile :: FilePath
      }
   | SrcFileResult
      {
         sfrFile :: FilePath
      }    
   | DestFileResult
      {
         dfrFile :: FilePath
      }
   | SrcDirFileMismatch
      {
         srcDir :: FilePath,
         destFile :: FilePath
      }
   | DestDirFileMismatch
      {
         destDir :: FilePath,
         srcFile :: FilePath
      }
   deriving(Show)


printResult :: Result -> String
printResult (DirResult sDir dDir) = printf "'%s' <=> '%s'" sDir dDir
printResult (SrcDirResult sDir) = printf "'%s' =>" sDir
printResult (DestDirResult dDir) = printf "<= '%s' " dDir
printResult (FileResult sFile _) = printf "%s" $ takeFileName sFile
printResult (SrcFileResult sFile) = printf "%s =>" $ takeFileName sFile
printResult (DestFileResult dFile) = printf "<= %s" $ takeFileName dFile
printResult (SrcDirFileMismatch sDir dFile) = printf "File directory mismatch '%s' '%s'" sDir dFile
printResult (DestDirFileMismatch dDir sFile) = printf "File directory mismatch '%s' '%s'" dDir sFile


compDirsIter :: Bool -> FilePath -> FilePath -> (Result -> IO ()) -> IO ()
compDirsIter recursive sDir dDir iterFun = do
   isSrcDir <- doesDirectoryExist sDir
   isSrcFile <- doesFileExist sDir
   isDestDir <- doesDirectoryExist dDir
   isDestFile <- doesFileExist dDir    
   
   case (isSrcDir, isSrcFile, isDestDir, isDestFile) of
      (True, False, True, False)  -> do
         iterFun $ DirResult sDir dDir  
         compDirsIter_ recursive sDir dDir iterFun
         
      (True, False, False, False) -> compOnlyDirIter recursive True sDir iterFun 
      (False, False, True, False) -> compOnlyDirIter recursive False dDir iterFun
      (True, False, False, True)  -> iterFun $ SrcDirFileMismatch sDir dDir
      (False, True, True, False)  -> iterFun $ DestDirFileMismatch dDir sDir
      _ -> error "Fatal error: The files system entry can be either a file or a directory."
      
   where
      
      compDirsIter_ :: Bool -> FilePath -> FilePath -> (Result -> IO ()) -> IO ()
      compDirsIter_ recursive sDir dDir iterFun = do
         srcDirConts <- getDirectoryContents sDir
         destDirConts <- getDirectoryContents dDir
         let 
            propFile file = file /= "." && file /= ".."
            propSrcDirConts = filter propFile srcDirConts
            propDestDirConts = filter propFile destDirConts
            intersectConts = propSrcDirConts `intersect` propDestDirConts
            onlySrcConts = propSrcDirConts \\ propDestDirConts
            onlyDestConts = propDestDirConts \\ propSrcDirConts
            
         compIntersectResults intersectConts
         compOnlyResults True onlySrcConts  
         compOnlyResults False onlyDestConts
         
         where
            compOnlyResults :: Bool -> [FilePath] -> IO ()
            compOnlyResults isSrc conts = forM_ conts $ \name -> do             
               let
                  dir = if isSrc then sDir else dDir 
                  fullName = dir </> name   
                  
               isFile <- doesFileExist fullName
               isDir <- doesDirectoryExist fullName
               
               case (isFile, isDir) of
                  (True, False) -> do
                     iterFun $ if isSrc then SrcFileResult fullName else DestFileResult fullName
                     
                  (False, True) -> do
                     iterFun $ if isSrc then SrcDirResult fullName else DestDirResult fullName
                     when recursive $ compOnlyDirIter True isSrc fullName iterFun

                  (False, False) -> return ()
                  
                  _ -> error "Fatal error: The files system entry can be either a file or a directory."         
         
         
            compIntersectResults :: [FilePath] -> IO ()
            compIntersectResults intersectConts = forM_ intersectConts $ \name -> do             
               let 
                  srcFullName = sDir </> name   
                  destFullName = dDir </> name
                  
               isSrcFile <- doesFileExist srcFullName
               isSrcDir <- doesDirectoryExist srcFullName
               isDestFile <- doesFileExist destFullName
               isDestDir <- doesDirectoryExist destFullName
               
               case (isSrcFile, isSrcDir, isDestFile, isDestDir) of
                  (True, False, True, False) -> iterFun $ FileResult srcFullName destFullName
                  
                  (False, True, False, True) -> do
                     iterFun $ DirResult srcFullName destFullName
                     when recursive $ compDirsIter_ True srcFullName destFullName iterFun
                  
                  (False, True, True, False)  -> iterFun $ SrcDirFileMismatch srcFullName destFullName
                  
                  (True, False, False, True)  -> iterFun $ DestDirFileMismatch destFullName srcFullName
                     
                  _ -> error "Fatal error: The files system entry can be either a file or a directory."
      
      
      compOnlyDirIter :: Bool -> Bool -> FilePath -> (Result -> IO ()) -> IO ()
      compOnlyDirIter recursive isSrc compDir iterFun = do
         dirConts <- getDirectoryContents compDir
         let 
            propFile file = file /= "." && file /= ".."
            propDirConts = map (compDir </>) . filter propFile $ dirConts
            
         files <- filterM doesFileExist propDirConts
         forM_ files $ \file -> 
            let 
               rs = if isSrc
                  then SrcFileResult file
                  else DestFileResult file
            in
               iterFun rs
                
         dirs <- filterM doesDirectoryExist propDirConts
         
         forM_ dirs $ \ dir ->
            let 
               rs = if isSrc
                  then SrcDirResult dir
                  else DestDirResult dir
            in do
               iterFun rs
               when recursive $ compOnlyDirIter True isSrc dir iterFun

------------------------------------------------------------------------------------------------------------------------