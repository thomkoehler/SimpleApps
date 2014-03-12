
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
import Data.Attoparsec.ByteString.Char8 as AC 

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
      parseBuildStartVS,
      parseBuildStart1,
      parseBuildStartBorland,
      parseBuildEndVS,
      parseBuildSuccessBorland,
      parseBuildFailedBorland 
   ]


parseBuildStartVS :: Parser BuildLogToken
parseBuildStartVS = do
   option () (intDecimal >> char '>' >> return ())
   _ <- string "------ Build started: Project: "
   name <- identifier
   return $ BuildStart $ name
   
   
parseBuildStart1 :: Parser BuildLogToken
parseBuildStart1 = do
   _ <- string " Creating "
   name <- identifier
   _ <- string " Project"
   return $ BuildStart $ name


parseBuildStartBorland :: Parser BuildLogToken
parseBuildStartBorland = do
   _ <- "Project"
   skipSpace
   projectPath <- stringLiteral
   _ <- " on node "
   _ <- intDecimal
   _ <- string " (default targets)."
   return $ BuildStart $ projectPath
   

parseBuildEndVS :: Parser BuildLogToken
parseBuildEndVS = do
   _ <- string "========== Build:"
   skipSpace
   _ <- intDecimal
   skipSpace
   _ <- string "succeeded,"
   skipSpace
   failed :: Int <- decimal
   return $ BuildEnd $ failed == 0 


parseBuildSuccessBorland :: Parser BuildLogToken
parseBuildSuccessBorland = do
   _ <- string "Build succeeded."
   return $ BuildEnd True
   
   
parseBuildFailedBorland :: Parser BuildLogToken
parseBuildFailedBorland = do
   _ <- string "Build FAILED."
   return $ BuildEnd False    

   
identifier :: Parser B.ByteString
identifier = do
   AB.takeWhile $ AB.inClass "A-Za-z0-9_."


intDecimal :: Parser Int
intDecimal = decimal


stringLiteral :: Parser B.ByteString
stringLiteral = do
   _ <- char '"'
   str <- AC.takeWhile (/= '"') 
   _ <- char '"'
   return str


main :: IO ()
main = do
      putStrLn "..."
   
      runResourceT $ 
         sourceFile "build_log_7.1.txt" 
         $$ lines 
         =$ (parseBuildLogTokens  =$= map (C.pack . show)) =$= lines'
         =$ sinkHandle stdout

      putStrLn "\nREADY"
         
   where
      lines' = map (C.append (C.pack "\n"))


-----------------------------------------------------------------------------------------------------------------------
