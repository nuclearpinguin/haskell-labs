{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import Web.Spock
import Web.Spock.Config

import Data.Aeson hiding (json) -- Because we use Web.Spock.json
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import GHC.Generics
import Network.HTTP.Types (Status, ok200, created201, badRequest400, notFound404)

import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Database.Persist hiding (get) -- To avoid a naming clash with Web.Spock.get
import qualified Database.Persist as P  -- We'll be using P.get later for GET /people/<id>.
import Database.Persist.Sqlite hiding (get)
import Database.Persist.TH


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json -- The json keyword will make Persistent generate sensible ToJSON and FromJSON instances for us.
  name Text
  age Int
  deriving Show
|]

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn


-- API app
type Api = SpockM SqlBackend () () ()
type ApiAction a = SpockAction SqlBackend () () a


-- API  handlers
-- Aeson.object :: [Pair] -> Value
-- Aeson.(.=) :: Text -> v -> Pair
errorHandler ::  Status ->  ActionCtxT () IO ()
errorHandler s 
    | s == notFound404 = json $ object
                        [ "result" .= String "failure"
                        , "msg" .= String "Page not found."]
    | otherwise = json $ object
                    [ "result" .= String "failure"
                    , "msg" .= String "Something very bad happened!"]


-- server
main :: IO ()
main = do
    -- Create db connection
    pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
    
    -- Add custom error handling
    spockCfg <- (defaultSpockCfg () (PCPool pool) ()) >>= \cfg -> 
        return $ cfg {spc_errorHandler = errorHandler}
    
    -- Migrate
    runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool

    runSpock 3000 (spock spockCfg app)


-- API
app :: Api
app = do
    get "people/list" $ do
        allPeople <- runSQL $ selectList [] [Asc PersonId]
        json allPeople
    get "people/user" $ do
        -- Query params TODO: how to get values?
        p <- params
        json p
    post "people" $ do
        maybePerson <- jsonBody :: ApiAction (Maybe Person)
        case maybePerson of
            Nothing -> setStatus badRequest400
            Just person -> do
                newId <- runSQL $ insert person
                setStatus created201 
