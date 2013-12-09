------------------------------------------------------------------------------------------------------------------------

module Main where

import Shell
import Control.Monad
import System.Environment(getArgs)
import System.FilePath
import Control.Monad.IO.Class
import Text.Printf

------------------------------------------------------------------------------------------------------------------------

project :: FilePath
project = "NetworkManager"

installDir = "D:\\NMInstall_%s_%s"

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
   let po = poFromArguments args
   
   setEnv "NM_INSTALL" $ printf installDir (show (poPlatform po)) (show (poConfig po))

--   echo "ver: $ver"
--   echo "tfs: $tfs"
--   echo "NM_INSTALL: $NM_INSTALL"
  
   let buildDir = tfs </> project </> ver
   cd buildDir
   
   forM_ buildDefs $ build args
   return ()
   
------------------------------------------------------------------------------------------------------------------------

build :: [String] -> String -> Shell ()
build args buildDef = do 
   _ <- sh "build.bat" $ buildDef : args
   return ()  

data Config = Release | Debug deriving Show

data Platform = X86 | X64 deriving Show
   
data ProgramOptions = ProgramOptions
   {
      poConfig :: Config,
      poPlatform :: Platform 
   }
   deriving Show

   
poFromArguments :: [String] -> ProgramOptions
poFromArguments args = foldl fun defPO args
   where
      defPO = ProgramOptions Release X86
      fun po arg =
         case arg of
            "-dbg" -> po { poConfig = Debug }
            "-x64" -> po { poPlatform = X64 }
            _      -> po

  
------------------------------------------------------------------------------------------------------------------------
