module Main where

import qualified Network.Wai as W
import Network.Wai.Handler.Warp (run)
import Rest.Driver.Wai
import Control.Monad.IO.Class
import Rest.Api
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Model
import qualified Api.Team as Team
import Types

site :: Router SiteApi SiteApi
site = root -/ team
  where
    team = route Team.resource

api :: Api SiteApi
api = [(mkVersion 1 0 0, Some1 site)]

application :: ServerData -> W.Application
application serverData = apiToApplication (runSiteApi serverData) api

main :: IO ()
main = do
  pool <- createSqlitePool "test.db3" 10
  flip runSqlPool pool $ do
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool $ do
        runMigration migrateAll
  run 3000 $ application (ServerData pool)