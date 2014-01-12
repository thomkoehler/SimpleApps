
------------------------------------------------------------------------------------------------------------------------

module Main where

import System.Directory(getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath((</>))
import Data.List(intersect, (\\))
import Control.Monad(forM_, filterM, when)

------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = compDirsIter True "/home/tkoehler/Temp/my-stuff" "/home/tkoehler/Temp/my-stuff" $ \res -> do
   putStrLn $ show res
   
   
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
   deriving(Show)


compDirsIter :: Bool -> FilePath -> FilePath -> (Result -> IO ()) -> IO ()
compDirsIter recursive sDir dDir iterFun = do
   isSrcDir <- doesDirectoryExist sDir
   isDestFile <- doesFileExist dDir    
   
   case (isSrcDir, isDestFile) of
      (True, True)  -> compDirsIter_ recursive sDir dDir iterFun
      (True, False) -> compOnlyDirIter recursive True sDir iterFun 
      (False, True) -> compOnlyDirIter recursive False dDir iterFun 
      _             -> return ()
      
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
         
         where
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
                  
                  (True, False, False, True) -> do
                     iterFun $ SrcFileResult srcFullName
                     iterFun $ DestDirResult destFullName
                     
                  (False, True, True, False) -> do
                     iterFun $ SrcDirResult srcFullName
                     iterFun $ DestFileResult destFullName
                     
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