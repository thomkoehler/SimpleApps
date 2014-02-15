
-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, EmptyDataDecls, FlexibleContexts, GADTs, OverloadedStrings #-}

module Main where


import Yesod
import Database.Persist.Sqlite
import Control.Monad.Logger
import Data.Conduit

-----------------------------------------------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Writer
    name String
    notes String
    UniqueWriterName name
    
Book
   title String
   notes String
   writer Int
   UniqueBookTitle title

|]

-----------------------------------------------------------------------------------------------------------------------


data Books = Books ConnectionPool


mkYesod "Books" [parseRoutes|
/ HomeR GET
/writer WriterR GET
/books BooksR GET
|]


instance Yesod Books

instance YesodPersist Books where
   type YesodPersistBackend Books = SqlPersistT
   
   runDB action = do
        Books pool <- getYesod
        runSqlPool action pool 


getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
<a href=@{WriterR}>Go to writer list!
<a href=@{BooksR}>Go to book list!
|]


getWriterR :: Handler Html
getWriterR = do 
   writers <- runDB $ selectList [] [Asc WriterName] 
   defaultLayout [whamlet|
<h1> Writers
$if null writers
   <p> There are no writers
$else
   <ul>
      $forall Entity writerId writer <- writers
         <li>#{show writerId} #{show $writerName writer}
|]


getBooksR :: Handler Html
getBooksR = defaultLayout [whamlet|Hello Books!|]

-----------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   let config = SqliteConf "books.db" 1
   pool <- createPoolConfig config
   runNoLoggingT $ runResourceT $ flip runSqlPool pool $ do
      runMigration migrateAll
      insert_ $ Writer "Alastair Reynolds" ""
      insert_ $ Writer "Philip K. Dick" ""
      insert_ $ Writer "Dann Simmons" ""
      insert_ $ Writer "Peter Hamilton" ""
      insert_ $ Writer "China Mieville" ""
      insert_ $ Writer "Stanislaw Lem" ""
      insert_ $ Writer "Strugatzki" ""
        
   warp 3000 $ Books pool


-----------------------------------------------------------------------------------------------------------------------