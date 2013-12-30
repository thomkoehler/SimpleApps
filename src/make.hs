#!/usr/bin/env runhaskell

module Main where

import Shell
import System.Environment
import Control.Monad
import Control.Monad.IO.Class
import System.IO
import Text.Printf
import System.FilePath

gitDirs =
   [
      "MiscTools",
      "Shell",
      "SimpleApps",
      "ParallelAndConcurrent",
      "HaskellTest",
      "FileFinder",
      "ImplFuncLang" </> "TemplInst",
      "SrcGen",
      "PathSearch"   
   ]

installDirs =
   [
      "MiscTools",
      "Shell",
      "PathSearch"   
   ]
   
   
gitRepoPattern = "https://thomkoehler:%s@github.com/thomkoehler/%s.git"

   
data ProgramOptions = ProgramOptions
   {
      poGitPull :: Bool,
      poInstall :: Bool,
      poGitPush :: Bool
   }
   
defPo = ProgramOptions False False False

   
getProgramOptions :: [String] -> ProgramOptions
getProgramOptions args = 
   let
      fun po arg = case arg of
         "pull"    -> po { poGitPull = True }
         "install" -> po { poInstall = True }
         "push"    -> po { poGitPush = True }
         _         -> error $ "Unknow argument found: " ++ arg
   in
      foldl fun defPo args
      
   

   
main :: IO ()
main = shell $ do
   args <- liftIO $ getArgs
   let pos = getProgramOptions args
   when (poGitPull pos) $ shInDirs gitDirs "git" ["pull"]
   when (poInstall pos) $ shInDirs installDirs "cabal" ["install"]
   when (poGitPush pos) $ push installDirs
  
      
shInDirs dirs cmd params = do
   pwdir <- pwd
   sub $
      forM_ dirs $ \dir -> do
         liftIO $ printf "change to %s\n" dir
         cd $ pwdir </> dir
         sh cmd params
         
         
push dirs = do
   liftIO $ putStr "Password: "
   liftIO $ hFlush stdout
   pass <- liftIO getLine 
   pwdir <- pwd
   sub $
      forM_ dirs $ \dir -> do
         liftIO $ printf "chenge to %s\n" dir
         cd $ pwdir </> dir
         let repName = takeFileName dir
         let gitUrl = printf gitRepoPattern pass repName
         sh "git" ["push", gitUrl]
   

   
   
   
   
   