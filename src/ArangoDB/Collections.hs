{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- See <https://docs.arangodb.com/3.3/HTTP/Collection/>.
module ArangoDB.Collections where

import Data.Proxy
import Data.String
import Servant.API
import ArangoDB.Types
import ArangoDB.Utils.Aeson
import ArangoDB.Utils.Client

-- * Creating

-- ** Create collection

type CreateCollection
  = "collection"
 :> ReqBody '[JSON] CreateCollectionRequest
 :> Post '[JSON] NoContent

createCollection :: CreateCollectionRequest -> ArangoClientM NoContent
createCollection = arangoClient (Proxy @CreateCollection)

data CreateCollectionRequest = CreateCollectionRequest
  { createCollectionJournalSize          :: Maybe Int
  , createCollectionReplicationFactor    :: Maybe Int
--   , createCollectionKeyOptions           :: Maybe CollectionKeyOptions
  , createCollectionName                 :: CollectionName
  , createCollectionWaitForSync          :: Maybe Bool
  , createCollectionDoCompact            :: Maybe Bool
  , createCollectionIsVolatile           :: Maybe Bool
--   , createCollectionShardKeys            :: Maybe CollectionShardKeys
  , createCollectionNumberOfShards       :: Maybe Int
  , createCollectionIsSystem             :: Maybe Bool
--   , createCollectionType                 :: Maybe CollectionType
  , createCollectionIndexBuckets         :: Maybe Int
  , createCollectionDistributeShardsLike :: Maybe CollectionName
  } deriving (Show)

instance IsString CreateCollectionRequest where
  fromString name = CreateCollectionRequest
    { createCollectionJournalSize          = Nothing
    , createCollectionReplicationFactor    = Nothing
  --   , createCollectionKeyOptions           = Nothing
    , createCollectionName                 = fromString name
    , createCollectionWaitForSync          = Nothing
    , createCollectionDoCompact            = Nothing
    , createCollectionIsVolatile           = Nothing
  --   , createCollectionShardKeys            = Nothing
    , createCollectionNumberOfShards       = Nothing
    , createCollectionIsSystem             = Just isSystem
  --   , createCollectionType                 = Nothing
    , createCollectionIndexBuckets         = Nothing
    , createCollectionDistributeShardsLike = Nothing
    }
    where
      isSystem = case name of
        '_':_ -> True
        _     -> False

-- * Getting information

-- ** Return information about a collection

type GetCollection
  = "collection"
 :> Capture "collection-name" CollectionName
 :> Get '[JSON] Collection

getCollection :: CollectionName -> ArangoClientM Collection
getCollection = arangoClient (Proxy @GetCollection)

-- Template Haskell derivations
deriveJSON' ''CreateCollectionRequest
