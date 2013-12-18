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

installDir = "D:\\NMInstall\\%s\\%s\\%s"
theGuardDir = "C:\\Program Files (x86)\\REALTECH\\theGuard!"

buildDefs :: [String]
buildDefs =
   [
      "SQLite",
      "Xerces",
      "log4cxx",
      "log4net",
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
      "NMDBCOMServiceCenterNMCopy",
      "CommonTools",
      "nmstring",
      "SharedBuffer",
      "SharedBufferExtension",
      "NMDBServerAssistantLayer",
      "ProcessAdminPlugins",
      "ScheduleServer",
      "RequestServerClient",
      "MultiSiteCommon",
      "RequestServer",
      "MultiSiteServer",
      "DBAdmin"
   ]

  
main :: IO ()
main = shell $ do
   ver <- getEnv_ "ver" "dev"
   tfs <- getEnv_ "tfs" "D:/Projects"

   args <- liftIO getArgs
   let po = poFromArguments args
   
   let instDir = printf installDir ver (show (poConfig po)) (show (poPlatform po))
   setEnv "NM_INSTALL" instDir
   setEnv "BCBLIB" $ "D:\\Libraries_" ++ ver

   echo "BCBLIB: $BCBLIB"
   echo "ver: $ver"
   echo "tfs: $tfs"
   echo "NM_INSTALL: $NM_INSTALL"
   echo $ show po

   if poCopy po
      then copyAll po instDir
      else buildAll tfs ver args
      
   where
   
      buildAll tfs ver args = do
         let buildDir = tfs </> project </> ver
         cd buildDir
         forM_ buildDefs $ build args

      copyAll po instDir = undefined
   
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
      poPlatform :: Platform ,
      poCopy :: Bool
   }
   deriving Show

   
poFromArguments :: [String] -> ProgramOptions
poFromArguments args = foldl fun defPO args
   where
      defPO = ProgramOptions Release X86 False
      fun po arg =
         case arg of
            "-dbg"  -> po { poConfig = Debug }
            "-x64"  -> po { poPlatform = X64 }
            "-copy" -> po { poCopy = True }
            _       -> po

poToBinDir :: ProgramOptions -> FilePath
poToBinDir po = case (poConfig po, poPlatform po) of
   (Release, X86) -> "bin" 
   (Debug, X86)   -> "bin_Debug"
   (Release, X64) -> "bin64" 
   (Debug, X64)   -> "bin64_Debug"
   _              -> error "ProgramOptions not supported"
   
------------------------------------------------------------------------------------------------------------------------
