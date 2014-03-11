
-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Prelude hiding(map, lines, unlines, takeWhile)
import Data.Conduit(runResourceT, ($$), (=$), (=$=), Conduit)
import Data.Conduit.Binary(sourceFile, sinkHandle, lines)
import Data.Conduit.List(map, concatMapAccum)
import System.IO(stdout)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Attoparsec.ByteString as AB
import Data.Attoparsec.ByteString.Char8

-----------------------------------------------------------------------------------------------------------------------

data BuildLogToken
   = BuildStart B.ByteString
   | BuildEnd Bool
   | None
   deriving Show


parseBuildLogTokens :: Monad m => Conduit B.ByteString m BuildLogToken
parseBuildLogTokens = concatMapAccum step ()
   where
      step line _ = case parse parseBuildLogToken line of
         Done _ token -> ((), [token])
         
         Partial cont -> case cont "" of
            Done _ token -> ((), [token])
            _            -> ((), [])
             
         _            -> ((), [])


parseBuildLogToken :: Parser BuildLogToken
parseBuildLogToken = choice 
   [
      parseBuildEnd0,
      parseBuildStart0,
      parseBuildEnd1, 
      parseBuildStart1
   ]


parseBuildStart0 :: Parser BuildLogToken
parseBuildStart0 = do
   _ <- string "------ Build started: Project: "
   name <- identifier
   return $ BuildStart $ name
   
   
parseBuildStart1 :: Parser BuildLogToken
parseBuildStart1 = do
   _ <- string " Creating "
   name <- identifier
   _ <- string " Project"
   return $ BuildStart $ name


parseBuildEnd0 :: Parser BuildLogToken
parseBuildEnd0 = do
   _ <- string "========== Build:"
   skipSpace
   _ :: Int <- decimal
   skipSpace
   _ <- string "succeeded,"
   skipSpace
   failed :: Int <- decimal
   return $ BuildEnd $ failed == 0 


parseBuildEnd1 :: Parser BuildLogToken
parseBuildEnd1 = do
   _ <- string "Build succeeded."
   return $ BuildEnd True  

   
identifier :: Parser B.ByteString
identifier = do
   AB.takeWhile $ AB.inClass "A-Za-z0-9_."


main :: IO ()
main = do
      putStrLn "..."
   
      runResourceT $ 
         sourceFile "build_log_7.0.txt" 
         $$ lines 
         =$ (parseBuildLogTokens  =$= map (C.pack . show)) =$= lines'
         =$ sinkHandle stdout

      putStrLn "\nREADY"
         
   where
      lines' = map (C.append (C.pack "\n"))


-----------------------------------------------------------------------------------------------------------------------
