
------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Time.Clock(UTCTime)
import System.Directory(getDirectoryContents, doesFileExist)
import Control.Monad(filterM)
import Data.List(sortBy)
import Data.Ord(Ordering(..), compare)

------------------------------------------------------------------------------------------------------------------------

main = putStrLn "Hallo"



data CompResult = CompResult
   {
      fileName :: !String,
      srcDir :: !FilePath,
      destDir :: !FilePath,
      srcModTime :: !UTCTime,
      destModTime :: !UTCTime
   }


compDirs :: FilePath -> FilePath -> IO [CompResult]
compDirs srcDir destDir = do
   srcDirConts <- getDirectoryContents srcDir
   destDirConts <- getDirectoryContents destDir
   let 
      propFile file = file /= "." && file /= ".."
      propSrcDirConts = filter propFile srcDirConts
      propDestDirConts = filter propFile destDirConts
      
   srcFiles <- filterM doesFileExist propSrcDirConts
   destFiles <- filterM doesFileExist propDestDirConts
   
   return []


compList :: Eq a => [a] -> [a] -> ([a], [a], [a])
compList fun l0 l1 = 
   let
       diff = intersectBy fun l0 l1
       s1 = sortBy fun l1
       compList' [] [] = ([], [], [])
       compList' (x:xs) (y:ys) = 
   in
      compList' s0 s1

   
------------------------------------------------------------------------------------------------------------------------