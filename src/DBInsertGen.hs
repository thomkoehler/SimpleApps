
module DBInsertGen where

import Database.HDBC.ODBC(connectODBC)
import Database.HDBC
import System.Environment(getArgs)
import Data.List(intercalate)
import Control.Monad(forM_)
import Data.ByteString.Char8(unpack)


main :: IO ()
main = do
   --(conn : _) <- getArgs
   let conn = "DSN=Main"
   
   dbConnection <- connectODBC conn
   printInserts dbConnection "ErmTreeentry"
   putStrLn ""
   printInserts dbConnection "ErmTree"
   putStrLn "" 
   
   
printInserts dbConnection tableName = do    
   tableDesc <- describeTable dbConnection tableName
   let 
      sqlSelect = createSelect tableName tableDesc
      insertPrefix = "INSERT INTO " ++ tableName ++ "(" ++ intercalate ", " (map fst tableDesc) ++ ") VALUES("
      colDescs = map snd tableDesc
   
   results <- quickQuery' dbConnection sqlSelect []
   
   forM_ results $ \result -> do
      putStrLn $ showRow insertPrefix colDescs result  
    
   return ()
   
   
createSelect :: String -> [(String, SqlColDesc)] -> String
createSelect tableName tableDesc = "SELECT " ++ (intercalate ", " (map fst tableDesc)) ++ " FROM " ++ tableName


showRow :: String -> [SqlColDesc] -> [SqlValue] -> String
showRow insertPrefix _ values = insertPrefix ++ intercalate ", " (map showValue values) ++ ");"
   where
      showValue (SqlString str) = "'" ++ str ++ "'"
      showValue (SqlByteString str) = "'" ++ unpack str ++ "'"
      showValue (SqlInt32 i) = show i
      showValue x = show x
