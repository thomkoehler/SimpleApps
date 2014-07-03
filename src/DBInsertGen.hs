
-----------------------------------------------------------------------------------------------------------------------

module DBInsertGen where

import Database.HDBC.ODBC(connectODBC)
import Database.HDBC
import System.Environment(getArgs)
import Data.List(intercalate)
import Control.Monad(forM_)
import Data.ByteString.Char8(unpack)
import qualified Data.ByteString as BS
import Data.Time.LocalTime
import Data.Time.Calendar
import Text.Printf
import Data.Fixed

-----------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   args <- getArgs
   if length args /= 2
      then usage
      else do
         let (conn : tableName : _) = args
         dbConnection <- connectODBC conn
         printInserts dbConnection tableName
   
   
usage :: IO ()
usage = do
   putStrLn "usage:"
   putStrLn "   DBInsertGen \"DSN=<name>\" <table-name>"

   
printInserts :: IConnection conn => conn -> String -> IO ()
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
createSelect tableName colNames = "SELECT " ++ intercalate ", " colNames ++ " FROM " ++ tableName


printInsertPrefix :: String -> [String] -> [SqlValue] -> IO ()
printInsertPrefix tableName colNames values = do
   putStr "INSERT INTO "
   putStr tableName
   putStr "("
   printIntercalate (putStr ", " ) (putStr . fst) $ filter notNull (zip colNames values)
   putStr ")"
   
   where
      notNull (_, SqlNull) = False
      notNull _ = True
    
      
printRowValues :: [SqlValue] -> IO ()
printRowValues [] = return ()
printRowValues values = printIntercalate (putStr ", ") printValue  $ filter notNull values  
   where
      notNull SqlNull = False
      notNull _ = True
   
      printValue (SqlString str) = putStr "'" >> putStr str >> putStr "'"
      printValue (SqlByteString str) = putStr "'" >> BS.putStr str >> putStr "'"
      printValue (SqlInt32 i) = putStr $ show i 
      printValue (SqlChar c) = putStr $ show $ fromEnum c
      printValue (SqlLocalTime lt) = printLocalTime lt
      printValue x = error $ "\nValue type " ++ show x ++ " not yet supported."
   
   
printIntercalate :: Monad m => m () -> (a -> m ()) -> [a] -> m ()
printIntercalate intercalateFun printFun [] = return () 
printIntercalate intercalateFun printFun (x : xs) = do
   printFun x
   forM_ xs $ \x -> intercalateFun >> printFun x    


printLocalTime :: LocalTime -> IO ()
printLocalTime (LocalTime ltDay ltTimeOfDay) = do
   let 
      (year, month, day) = toGregorian ltDay
      (TimeOfDay hour min sec) = ltTimeOfDay
   printf "CONVERT (DATETIME, '%04d-%02d-%02d %02d:%02d:%s', 21)" year month day hour min (showFixed True sec)

   
-----------------------------------------------------------------------------------------------------------------------