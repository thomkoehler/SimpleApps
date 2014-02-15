
------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding(lines, map, scanl)
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List
import System.IO
import Data.ByteString hiding(map)
import Data.Binary
import Data.Monoid ((<>))
import Data.Attoparsec
import Data.Attoparsec.ByteString
import Data.Attoparsec.Char8


------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = runResourceT $ 
   sourceFile "CinemaOTNode.h" 
   $$ lines 
--   =$ lineCounter 
--   =$ intToString
   =$ numberLine
   =$ sinkHandle stdout
   
   

numberLine :: Monad m => Conduit ByteString m ByteString
numberLine = concatMapAccum step 0 where
  --format input lno = pack (show lno) <> pack " " <> input <> pack "\n"
  step input lno = (lno+1, ["Hallo"])



parseDefine :: Parser ()
parseDefine = skipSpace >> string "#define" >> return ()

------------------------------------------------------------------------------------------------------------------------
