module Main where

import qualified Network.Wai as W
import qualified Network.Wai.Middleware.Cors as W
import qualified Network.HTTP.Types.Method as W
import Network.Wai.Handler.Warp (run)
import Rest.Driver.Wai (apiToApplication)
import Rest.Api (Router, root, route, Api, mkVersion, Some1 (Some1), (-/), (--/))
import Database.Persist.Sqlite (createSqlitePool, runSqlPool, runMigration)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Type.Api (ServerData (..), SiteApi, runSiteApi)
import Type.Generated (migrateAll)
import qualified Api.Team as Team
import qualified Api.Sprint as Sprint

site :: Router SiteApi SiteApi
site = root -/ team --/ sprint
  where
    team = route Team.resource
    sprint = route Sprint.resource

api :: Api SiteApi
api = [(mkVersion 1 0 0, Some1 site)]

application :: ServerData -> W.Application
application serverData = apiToApplication (runSiteApi serverData) api

corsPolicy :: W.CorsResourcePolicy
corsPolicy = W.CorsResourcePolicy
  { W.corsOrigins = Nothing
  , W.corsMethods = [ W.methodGet
                    , W.methodPost
                    , W.methodHead
                    , W.methodPut
                    , W.methodDelete
                    , W.methodOptions
                    ]
  , W.corsRequestHeaders = ["Content-Type"]
  , W.corsExposedHeaders = Nothing
  , W.corsMaxAge = Just 3600
  , W.corsVaryOrigin = True
  , W.corsRequireOrigin = False
  , W.corsIgnoreFailures = False
  }

main :: IO ()
main = do
  pool <- createSqlitePool "test.db3" 10
  flip runSqlPool pool $ do
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool $ do
        runMigration migrateAll
  run 3000 $ W.cors (const $ Just corsPolicy) $ application (ServerData pool)