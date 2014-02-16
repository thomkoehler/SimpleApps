
------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding(lines, map, scanl, takeWhile)
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List
import Data.ByteString hiding(map, pack)
import Data.Binary
import Data.Attoparsec
import Control.Applicative((<|>))
import qualified Data.Char as C
import qualified Data.Attoparsec.Char8 as A
import qualified Data.Attoparsec.ByteString as B
import qualified Data.Map.Strict as Map
import Control.Monad(forM_)
import Data.Monoid((<>))
import Data.ByteString.Char8(pack)
import System.IO(stdout)

------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = conduitTest


conduitTest :: IO ()
conduitTest = do
   runResourceT $ 
      sourceFile "CinemaOTNode.h" 
      $$ lines 
      =$ parserConduit
      =$ sinkHandle stdout

   
parserConduit :: Monad m => Conduit ByteString m ByteString
parserConduit = concatMapAccum step Map.empty
   where
      step line defines = case parse (parseLine defines) line of
         Done _ (StringDefine name value) -> (Map.insert name value defines, [])   
         Done _ (PairDef name value)      -> (defines, [name <> pack ": " <> value <> pack "\n"])
         _                                -> (defines, [])


data ParseResult 
   = StringDefine ByteString ByteString
   | PairDef ByteString ByteString


parseLine :: Map.Map ByteString ByteString -> Parser ParseResult
parseLine defines = parseDefine <|> parsePairDef defines
   

parseDefine :: Parser ParseResult
parseDefine = do
   char_ '#'
   string_ "define"
   ident <- identifier
   strLit <- stringLiteral
   return $ StringDefine ident strLit


parsePairDef :: Map.Map ByteString ByteString -> Parser ParseResult
parsePairDef defines = do
   string_ "std::make_pair"
   char_ '('
   identName <- identifier
   char_ ','
   identValue <- identifier
   char_ ')'
   case Map.lookup identValue defines of
      Just value -> return $ PairDef identName value
      _          -> error $ "value " ++ show identValue ++ " not found."


identifier :: Parser ByteString
identifier = do
   A.skipSpace 
   A.takeWhile $ A.inClass "A-Za-z0-9_"


stringLiteral :: Parser ByteString
stringLiteral = do
   char_ '"'
   str <- B.takeWhile (/= (ascii '"')) 
   _ <- A.char '"'
   return str
   
   
char_ :: Char -> Parser ()
char_ c = do
   A.skipSpace
   _ <- A.char c
   return ()
   
   
string_ :: ByteString -> Parser ()
string_ str = do
   A.skipSpace
   _ <- A.string str
   return ()


ascii :: Char -> Word8
ascii = fromIntegral . C.ord

------------------------------------------------------------------------------------------------------------------------

toChar :: Word8 -> Char
toChar = C.chr . fromIntegral


sinkTest :: IO ()
sinkTest = do
   (SinkResult _ nameValues) <-runResourceT $ 
      sourceFile "CinemaOTNode.h" 
      $$ lines 
      =$ parserSink
      
   forM_ nameValues $ \(name, value) -> do
      Data.ByteString.putStr name
      Prelude.putStr ": "
      Data.ByteString.putStrLn value
      
      
data SinkResult = SinkResult 
   {
      srDefines :: Map.Map ByteString ByteString,
      srNameValues :: [(ByteString, ByteString)]
   }


parserSink :: Monad m => Sink ByteString m SinkResult
parserSink = fold step emptySinkResult
   where
      emptySinkResult = SinkResult Map.empty []
   
      step prevSinkResult@(SinkResult defines nameValues) line = 
         case parse (parseLine defines) line of
            Done _ (StringDefine name value) -> prevSinkResult { srDefines =  Map.insert name value defines }
            Done _ (PairDef name value)      -> prevSinkResult { srNameValues = (name, value) : nameValues }
            _                                -> prevSinkResult

------------------------------------------------------------------------------------------------------------------------
