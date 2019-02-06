{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module ArangoDB.Utils.Client where

import           Control.Monad.Reader
import           Data.Aeson            (ToJSON)
import           Data.Coerce           (coerce)
import           Data.Functor          (void)
import           Data.Monoid           ((<>))
import           Data.Proxy
import           Network.HTTP.Client   (defaultManagerSettings, newManager)
import           Servant.API.BasicAuth
import           Servant.Client
import           Servant.Client.Core
import           Text.Show.Pretty      (pPrint)

import           ArangoDB.Utils.Aeson  (pPrintJSON)

data ArangoClientConfig = ArangoClientConfig
  { arangoHost     :: String
  , arangoPort     :: Int
  , arangoDatabase :: Maybe String
  , arangoAuth     :: Maybe BasicAuthData
  }

-- | Default ArangoDB client configuration.
--
-- @
-- defaultArangoClientConfig = ArangoClientConfig
--   { 'arangoHost'     = "localhost"
--   , 'arangoPort'     = 8529
--   , 'arangoDatabase' = Nothing
--   , 'arangoAuth'     = Just ('BasicAuthData' "root" "")
--   }
-- @
defaultArangoClientConfig :: ArangoClientConfig
defaultArangoClientConfig = ArangoClientConfig
  { arangoHost     = "localhost"
  , arangoPort     = 8529
  , arangoDatabase = Nothing
  , arangoAuth     = Just (BasicAuthData "root" "")
  }

-- | ArangoDB HTTP API 'BaseUrl'.
--
-- >>> showBaseUrl $ arangoBaseUrl defaultArangoClientConfig
-- "http://localhost:8529/_api"
arangoBaseUrl :: ArangoClientConfig -> BaseUrl
arangoBaseUrl ArangoClientConfig{..} = BaseUrl Http arangoHost arangoPort path
  where
    path = case arangoDatabase of
      Nothing -> "/_api"
      Just db -> "/_db/" <> db <> "/_api"

-- | 'ClientEnv' for ArangoDB HTTP API.
arangoClientEnv :: ArangoClientConfig -> IO ClientEnv
arangoClientEnv config = ClientEnv
  <$> newManager defaultManagerSettings
  <*> pure (arangoBaseUrl config)
  <*> pure Nothing

-- | Run ArangoDB HTTP API requests for a given ArangoDB server.
runArangoClientM :: ArangoClientConfig -> ArangoClientM a -> IO (Either ServantError a)
runArangoClientM config m = do
  env <- arangoClientEnv config
  runClientM (coerce m (arangoAuth config)) env

arangoClient :: (HasClient ArangoClientM api) => Proxy api -> Client ArangoClientM api
arangoClient api = api `clientIn` (Proxy :: Proxy ArangoClientM)

runDefault :: ArangoClientM a -> IO (Either ServantError a)
runDefault = runArangoClientM defaultArangoClientConfig

runDefault_ :: ArangoClientM a -> IO ()
runDefault_ = void . runDefault

runDefaultJSON :: ToJSON a => ArangoClientM a -> IO ()
runDefaultJSON m = do
  res <- runDefault m
  case res of
    Left err -> print err
    Right js -> pPrintJSON js

runDefaultPretty :: Show a => ArangoClientM a -> IO ()
runDefaultPretty m = runDefault m >>= pPrint

newtype ArangoClientT m a = ArangoClientT (ReaderT (Maybe BasicAuthData) m a)
  deriving (Functor, Applicative, Monad)

type ArangoClientM = ArangoClientT ClientM

instance RunClient m => RunClient (ArangoClientT m) where
  runRequest req = ArangoClientT $ ReaderT $ \mauth ->
    runRequest $ maybe id basicAuthReq mauth $ req
  throwServantError = ArangoClientT . ReaderT . const . throwServantError
  streamingRequest req = ArangoClientT $ ReaderT $ \mauth ->
    streamingRequest $ maybe id basicAuthReq mauth $ req
