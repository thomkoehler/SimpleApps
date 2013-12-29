#!/usr/bin/env runhaskell

module Main where

import Shell
import System.Environment
import Control.Monad
import Control.Monad.IO.Class

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
   
data ProgramOptions = ProgramOptions
   {
      poGitPull :: Bool,
      poInstall :: Bool
   }
   
defPo = ProgramOptions False False

   
getProgramOptions :: [String] -> ProgramOptions
getProgramOptions args = 
   let
      fun po arg = case arg of
         "pull"    -> po { poGitPull = True }
         "install" -> po { poInstall = True }
         _         -> error $ "Unknow argument found: " ++ arg
   in
      foldl fun defPo args
      
   

   
main :: IO ()
main = shell $ do
   args <- liftIO $ getArgs
   let pos = getProgramOptions args
   when (poGitPull pos) $ shInDirs gitDirs "git" ["pull"]
   when (poInstall pos) $ shInDirs installDirs "cabal" ["install"]
  
      
shInDirs dirs cmd params = do
   pwdir <- pwd
   sub $
      forM_ dirs $ \dir -> do
         cd $ pwdir </> dir
         sh cmd params
   