
-----------------------------------------------------------------------------------------------------------------------

module Main where

import Prelude hiding(map, lines, unlines)
import Data.Conduit(runResourceT, ($$), (=$), (=$=), Conduit)
import Data.Conduit.Binary(sourceFile, sinkHandle, lines)
import Data.Conduit.List(map, concatMapAccum)
import System.IO(stdout)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Text.Regex.Posix

-----------------------------------------------------------------------------------------------------------------------

regexBuildStarted :: Regex
regexBuildStarted = makeRegex "------ Build started: Project: ([a-zA-z]*)"



data BuildLogToken
   = BuildStart B.ByteString
   | BuildEnd Bool
   | None
   deriving Show



main :: IO ()
main = runResourceT $ 
   sourceFile "build_log_7.0.txt" 
   $$ lines 
   =$ (parseBuildLogToken  =$= map (C.pack . show)) =$= lines'
   =$ sinkHandle stdout
   where
      lines' = map (C.append (C.pack "\n"))

  

parseBuildLogToken :: Monad m => Conduit B.ByteString m BuildLogToken
parseBuildLogToken = concatMapAccum step ()
   where
      step line _ = 
         case  regexParser regexs line of
            BuildStart name -> ((), [BuildStart name])
            _               -> ((), [])
            

regexs = 
   [
      ("^------ Build started: Project: ([a-zA-z]*)", BuildStart),
      ("^\\*\\*\\*  Building ([a-zA-z]*)", BuildStart)
   ]


regexParser funs line = foldl step None funs
   where
      step prevToken (regex, createFun) = 
         case prevToken of
            None -> 
               case line =~ regex of
                  [_ : name : _] -> createFun name
                  _              -> None
               
            _    -> prevToken
         

-----------------------------------------------------------------------------------------------------------------------
