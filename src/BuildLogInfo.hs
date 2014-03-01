
-----------------------------------------------------------------------------------------------------------------------

module Main where

import Prelude hiding(map, lines)
import Data.Conduit(runResourceT, ($$), (=$), (=$=), Conduit)
import Data.Conduit.Binary(sourceFile, sinkHandle, lines)
import Data.Conduit.List(map, concatMapAccum)
import System.IO(stdout)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Text.Regex.Posix

-----------------------------------------------------------------------------------------------------------------------


data BuildLogToken
   = BuildStart B.ByteString
   | BuildEnd Bool
   | None
   deriving Show



main :: IO ()
main = runResourceT $ 
   sourceFile "build_log_7.0.txt" 
   $$ lines 
   =$ (parseBuildLogToken  =$= map (C.pack . show))
   =$ sinkHandle stdout


parseBuildLogToken :: Monad m => Conduit B.ByteString m BuildLogToken
parseBuildLogToken = concatMapAccum step ()
   where
      step line _ = 
         case parseLine line of
            BuildStart name -> ((), [BuildStart name])
            _               -> ((), [])
            


parseLine :: B.ByteString -> BuildLogToken
parseLine line =
   case line =~ "------ Build started: Project: ([a-zA-z]*)" of
      [_ : projectName : _] -> BuildStart projectName
      _                     -> None


-----------------------------------------------------------------------------------------------------------------------
