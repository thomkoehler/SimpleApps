------------------------------------------------------------------------------------------------------------------------

module Main where

import Shellish
import Control.Monad
import System.Environment(getArgs)

------------------------------------------------------------------------------------------------------------------------

project :: FilePath
project = "NetworkManager"


buildDefs :: [String]
buildDefs =
   [
      "Version",
      "Helper",
      "NMDBServerBase",
      "NMDBA",
      "NMDBServer.Net",
      "TextResource",
      "ProcessMonitor",
      "NMDBServer",
      "NMTextCreator",
      "ProcessAdmin",
--      "ReportingtypeCompression",
      "NMDBCOMServiceCenterNMCopy",
--      "CopyRTL",
      "CommonTools",
--      "EventServerClient",
--      "Scheduler",
--      "tGProtocolLib",
      "nmstring",
      "SharedBuffer",
      "SharedBufferExtension",
      "NMDBServerAssistantLayer"
   ]


main :: IO ()
main = shellish $ do
   ver <- getEnv "ver" "dev"
   tfs <- getEnv "tfs" "D:/Projects"
   
   args <- liftIO getArgs
   
  
   let buildDir = tfs </> project </> ver
   cd buildDir
   
   forM_ buildDefs $ build args
   return ()

------------------------------------------------------------------------------------------------------------------------

build :: [String] -> String -> ShIO ()
build args buildDef = do 
   _ <- shell "build.bat" $ buildDef : args
   return ()  


shell :: String -> [String] -> ShIO String
shell cmd args = do
   comspec <- getenv "ComSpec"
   run comspec $ "/C" : cmd : args
 
  
getEnv :: String -> String -> ShIO String
getEnv name def = do
   val <- getenv name
   if null val
      then return def
      else return val

------------------------------------------------------------------------------------------------------------------------
