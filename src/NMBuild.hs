------------------------------------------------------------------------------------------------------------------------

module Main where

import Shell
import Control.Monad
import System.Environment(getArgs)
import System.FilePath
import Control.Monad.IO.Class

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
main = shell $ do
   ver <- getEnv_ "ver" "dev"
   tfs <- getEnv_ "tfs" "D:/Projects"
   
   args <- liftIO getArgs
   
  
   let buildDir = tfs </> project </> ver
   cd buildDir
   
   forM_ buildDefs $ build args
   return ()
   
------------------------------------------------------------------------------------------------------------------------

build :: [String] -> String -> Shell ()
build args buildDef = do 
   _ <- sh "build.bat" $ buildDef : args
   return ()  

------------------------------------------------------------------------------------------------------------------------
