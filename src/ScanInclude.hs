
------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding(lines, map, scanl, takeWhile)
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List
import Data.ByteString hiding(map)
import Data.Binary
import Data.Attoparsec
import Control.Applicative((<|>))
import qualified Data.Char as C
import qualified Data.Attoparsec.Char8 as A
import qualified Data.Attoparsec.ByteString as B
import qualified Data.Map.Strict as Map
import Control.Monad(forM_)

------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
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


data ParseResult 
   = StringDefine ByteString ByteString
   | PairDef ByteString ByteString


parseLine :: Map.Map ByteString ByteString -> Parser ParseResult
parseLine defines = parseDefine <|> parsePairDef defines
   

parseDefine :: Parser ParseResult
parseDefine = do
   A.skipSpace
   _ <- A.char '#'
   A.skipSpace
   _ <- string "define"
   A.skipSpace
   ident <- identifier
   A.skipSpace
   strLit <- stringLiteral
   return $ StringDefine ident strLit


parsePairDef :: Map.Map ByteString ByteString -> Parser ParseResult
parsePairDef defines = do
   A.skipSpace
   _ <- string "std::make_pair"
   A.skipSpace
   _ <- A.char '('
   A.skipSpace
   identName <- identifier
   A.skipSpace
   _ <- A.char ','
   A.skipSpace
   identValue <- identifier
   A.skipSpace
   _ <- A.char ')'
   case Map.lookup identValue defines of
      Just value -> return $ PairDef identName value
      _          -> error $ "value " ++ show identValue ++ " not found."


identifier :: Parser ByteString
identifier = A.takeWhile $ A.inClass "A-Za-z0-9_"


stringLiteral :: Parser ByteString
stringLiteral = do
   _ <- A.char '"'
   str <- B.takeWhile (/= (ascii '"')) 
   _ <- A.char '"'
   return str


toChar :: Word8 -> Char
toChar = C.chr . fromIntegral


ascii :: Char -> Word8
ascii = fromIntegral . C.ord

------------------------------------------------------------------------------------------------------------------------

{--

numberLine :: Monad m => Conduit ByteString m ByteString
numberLine = concatMapAccum step 0 where
  format input lno = pack (show lno) <> pack " " <> input <> pack "\n"
  step input lno = (lno + 1, ["Hallo\n"])

--}
------------------------------------------------------------------------------------------------------------------------
