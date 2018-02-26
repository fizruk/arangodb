{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- See <https://docs.arangodb.com/3.3/HTTP/Collection/>.
module ArangoDB.Collections where

import Data.Aeson.WithField (WithFields(..), OnlyField(..))
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
 :> Post '[JSON] (CollectionInfo `WithFields` CollectionProperties)

createCollection
  :: CreateCollectionRequest
  -> ArangoClientM (CollectionInfo `WithFields` CollectionProperties)
createCollection = arangoClient (Proxy @CreateCollection)

data CreateCollectionRequest = CreateCollectionRequest
  { createCollectionJournalSize          :: Maybe Int
  , createCollectionReplicationFactor    :: Maybe Int
  , createCollectionKeyOptions           :: Maybe CollectionKeyOptions
  , createCollectionName                 :: CollectionName
  , createCollectionWaitForSync          :: Maybe Bool
  , createCollectionDoCompact            :: Maybe Bool
  , createCollectionIsVolatile           :: Maybe Bool
  , createCollectionShardKeys            :: Maybe [DocumentAttrName]
  , createCollectionNumberOfShards       :: Maybe Int
  , createCollectionIsSystem             :: Maybe Bool
  , createCollectionType                 :: Maybe CollectionType
  , createCollectionIndexBuckets         :: Maybe Int
  , createCollectionDistributeShardsLike :: Maybe CollectionName
  } deriving (Show)

-- | System collections start with an underscore:
--
-- >>> createCollectionIsSystem "_example"
-- Just True
--
-- >>> createCollectionIsSystem "example"
-- Just False
instance IsString CreateCollectionRequest where
  fromString name = CreateCollectionRequest
    { createCollectionJournalSize          = Nothing
    , createCollectionReplicationFactor    = Nothing
    , createCollectionKeyOptions           = Nothing
    , createCollectionName                 = fromString name
    , createCollectionWaitForSync          = Nothing
    , createCollectionDoCompact            = Nothing
    , createCollectionIsVolatile           = Nothing
    , createCollectionShardKeys            = Nothing
    , createCollectionNumberOfShards       = Nothing
    , createCollectionIsSystem             = Just isSystem
    , createCollectionType                 = Nothing
    , createCollectionIndexBuckets         = Nothing
    , createCollectionDistributeShardsLike = Nothing
    }
    where
      isSystem = case name of
        '_':_ -> True
        _     -> False

-- ** Drop a collection

type DropCollection
  = "collection"
 :> Capture "collection-name" CollectionName
 :> QueryParam "isSystem" Bool
 :> Delete '[JSON] (OnlyField "id" CollectionId)

dropCollection
  :: CollectionName
  -> Maybe Bool      -- ^ Is this a system collection?
  -> ArangoClientM CollectionId
dropCollection name isSystem = unOnlyField
  <$> arangoClient (Proxy @DropCollection) name isSystem

-- ** Truncate a collection

type TruncateCollection
  = "collection"
 :> Capture "collection-name" CollectionName
 :> "truncate"
 :> Put '[JSON] CollectionInfo

truncateCollection
  :: CollectionName
  -> ArangoClientM CollectionInfo
truncateCollection = arangoClient (Proxy @TruncateCollection)

-- * Getting information

-- ** Return information about a collection

type GetCollection
  = "collection"
 :> Capture "collection-name" CollectionName
 :> Get '[JSON] CollectionInfo

getCollection :: CollectionName -> ArangoClientM CollectionInfo
getCollection = arangoClient (Proxy @GetCollection)

-- ** Read properties of a collection

type GetCollectionProperties
  = "collection"
 :> Capture "collection-name" CollectionName
 :> "properties"
 :> Get '[JSON] (CollectionInfo `WithFields` CollectionProperties)

getCollectionProperties
  :: CollectionName -> ArangoClientM (CollectionInfo `WithFields` CollectionProperties)
getCollectionProperties = arangoClient (Proxy @GetCollectionProperties)

-- | Like 'getCollectionProperties' but returns just 'CollectionProperties'.
getCollectionProperties' :: CollectionName -> ArangoClientM CollectionProperties
getCollectionProperties' name = extractProperties <$> getCollectionProperties name
  where
    extractProperties (WithFields _ props) = props

-- ** Return number of documents in a collection

type GetCollectionCount
  = "collection"
 :> Capture "collection-name" CollectionName
 :> "count"
 :> Get '[JSON] (OnlyField "count" Integer `WithFields` CollectionInfo `WithFields` CollectionProperties)

getCollectionCount
  :: CollectionName -> ArangoClientM (OnlyField "count" Integer `WithFields` CollectionInfo `WithFields` CollectionProperties)
getCollectionCount = arangoClient (Proxy @GetCollectionCount)

-- | Like 'getCollectionCount' but returns just the count.
getCollectionCount' :: CollectionName -> ArangoClientM Integer
getCollectionCount' name = extractCount <$> getCollectionCount name
  where
    extractCount (WithFields (WithFields (OnlyField n) _) _) = n

-- Template Haskell derivations
deriveJSON' ''CreateCollectionRequest

-- ** Read all collections

type GetAllCollections
  = "collection"
 :> QueryParam "excludeSystem" Bool
 :> Get '[JSON] (OnlyField "result" [CollectionInfo])

-- | Get all collections (including system collections).
getAllCollections :: ArangoClientM [CollectionInfo]
getAllCollections = getAllCollectionsClient Nothing

-- | Get all non-system collections.
getAllNonSystemCollections :: ArangoClientM [CollectionInfo]
getAllNonSystemCollections = getAllCollectionsClient (Just True)

getAllCollectionsClient
  :: Maybe Bool   -- ^ Exclude system collections?
  -> ArangoClientM [CollectionInfo]
getAllCollectionsClient excludeSystem
  = unOnlyField <$> arangoClient (Proxy @GetAllCollections) excludeSystem
