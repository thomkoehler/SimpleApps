
------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Prelude hiding(FilePath)
import Shelly
import Data.Maybe
import Data.Text.Lazy hiding(map)
import Control.Monad
import System.Environment(getArgs)

------------------------------------------------------------------------------------------------------------------------

project :: FilePath
project = "NetworkManager"


buildDefs :: [Text]
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
main = shelly $ do
   ver <- getEnv "ver" "dev"
   tfs <- getEnv "tfs" "D:/Projects"
   
   args <- liftIO getArgs
   let targs = map pack args
   
   liftIO $ putStrLn $ show targs
   
   let buildDir = tfs </> project </> ver
   cd buildDir
   
   forM_ buildDefs $ build targs
   return ()

------------------------------------------------------------------------------------------------------------------------

build :: [Text] -> Text -> Sh ()
build args buildDef = do
   let
      properArgs = (toStrict buildDef) : (map toStrict args)
   liftIO $ putStrLn $ show properArgs       
   run_ "build.bat" properArgs 

  
getEnv :: Text -> Text -> Sh String
getEnv name def = do
   maybeValue <- get_env $ toStrict name
   return $ unpack $ fromStrict $ fromMaybe (toStrict def) maybeValue
   
------------------------------------------------------------------------------------------------------------------------
