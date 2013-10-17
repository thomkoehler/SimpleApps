------------------------------------------------------------------------------------------------------------------------

 {-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import Data.ByteString.Char8()
import Control.Monad.Trans.Resource(runResourceT, allocate, release)
import Control.Monad.Trans.Class(lift)
import System.IO(openFile, hClose, IOMode(ReadMode), hIsEOF, hSeek, SeekMode(RelativeSeek))
import Data.ByteString.Search(breakOn)
import Control.Monad(forM_, when)
import System.Directory(getDirectoryContents, doesDirectoryExist)
import System.FilePath((</>))

------------------------------------------------------------------------------------------------------------------------

currBufferSize :: Int
currBufferSize = 10240

------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   foreachFile "..//" (fileFun "class") True
   return ()  
   where
      fileFun :: B.ByteString -> FilePath -> IO ()
      fileFun text fileName = do
         found <- textFindInFile text currBufferSize fileName
         when found $ putStrLn fileName


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
foreachFile topdir fileFun recursive = do
   names <- getDirectoryContents topdir
   let properNames = filter (`notElem` [".", ".."]) names
   forM_ properNames $ \name -> do
      let path = topdir </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory && recursive
         then foreachFile path fileFun True
         else fileFun path
      
------------------------------------------------------------------------------------------------------------------------