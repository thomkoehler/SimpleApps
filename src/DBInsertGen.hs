
-----------------------------------------------------------------------------------------------------------------------

module DBInsertGen where

import Database.HDBC.ODBC(connectODBC)
import Database.HDBC
import System.Environment(getArgs)
import Data.List(intercalate)
import Control.Monad(forM_)
import Data.ByteString.Char8(unpack)
import qualified Data.ByteString as BS

-----------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   args <- getArgs
   if length args /= 2
      then usage
      else do
         (conn : tableName : _) <- getArgs
         dbConnection <- connectODBC conn
         printInserts dbConnection tableName
   
   
usage :: IO ()
usage = do
   putStrLn "usage:"
   putStrLn "   DBInsertGen \"DSN=<name>\" <table-name>"

   
printInserts dbConnection tableName = do    
   tableDesc <- describeTable dbConnection tableName
   let 
      colNames = map fst tableDesc
      sqlSelect = createSelect tableName colNames
   
   results <- quickQuery' dbConnection sqlSelect []
   forM_ results $ \result -> do
      printInsertPrefix tableName colNames result
      putStr " VALUES("
      printRowValues result  
      putStrLn ");"
    
   return ()
   
   
createSelect :: String -> [String] -> String
createSelect tableName colNames = "SELECT " ++ (intercalate ", " colNames) ++ " FROM " ++ tableName


printInsertPrefix :: String -> [String] -> [SqlValue] -> IO ()
printInsertPrefix tableName colNames values = do
   putStr "INSERT INTO "
   putStr tableName
   putStr "("
   printRowNames (filter notNull (zip colNames values))
   putStr ")"
   
   where
      notNull (_, SqlNull) = False
      notNull _ = True
   
      printRowNames [] = return ()
      printRowNames ((n, _) : ns) = do
         putStr n
         forM_ ns $ \(n, _) -> putStr ", " >> putStr n
    
      
printRowValues :: [SqlValue] -> IO ()
printRowValues [] = return ()
printRowValues values = printRowValues' (filter notNull values)  
   where
      notNull SqlNull = False
      notNull _ = True
   
      printRowValues' [] = return () 
      printRowValues' (v : vs) = do
         printValue v
         forM_ vs $ \value -> putStr ", " >> printValue value
   
      printValue (SqlString str) = putStr "'" >> putStr str >> putStr "'"
      printValue (SqlByteString str) = putStr "'" >> BS.putStr str >> putStr "'"
      printValue (SqlInt32 i) = putStr $ show i 
      printValue (SqlChar c) = putStr $ show $ fromEnum c
      printValue x = putStr $ show x
   
-----------------------------------------------------------------------------------------------------------------------