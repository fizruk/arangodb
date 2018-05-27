{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
-- |
-- See <https://docs.arangodb.com/3.3/HTTP/Collection/>.
module ArangoDB.Internal.Collections where

import           ArangoDB.Utils.Aeson
import           ArangoDB.Utils.Client
import           ArangoDB.Utils.Enum
import           Data.Aeson
import           Data.Aeson.WithField  (OnlyField (..), WithFields (..))
import           Data.Proxy
import           Data.String           (IsString (..))
import           Data.Text             (Text)
import           Generics.SOP          (Generic)
import qualified GHC.Generics          as GHC
import           Servant.API

-- $setup
-- >>> :set -XOverloadedStrings

-- * Creating

-- ** Create collection

type CreateCollection
  = "collection"
 :> ReqBody '[JSON] CreateCollectionRequest
 :> Post '[JSON] (CollectionInfo `WithFields` CollectionProperties)

-- | Create a new collection.
--
-- >>> runDefaultPretty $ createCollection "hs-example"
-- Right
--   (WithFields
--      CollectionInfo
--        { collectionId = "..."
--        , collectionName = "hs-example"
--        , collectionStatus = CollectionLoaded
--        , collectionType = DocumentCollection
--        , collectionIsSystem = False
--        }
--      CollectionProperties
--        { collectionWaitForSync = False
--        , collectionDoCompact = True
--        , collectionJournalSize = ...
--        , collectionKeyOptions =
--            CollectionKeyOptions
--              { collectionKeyType = KeyGeneratorTraditional
--              , collectionKeyAllowUserKeys = True
--              }
--        , collectionIsVolatile = False
--        , collectionNumberOfShards = Nothing
--        , collectionShardKeys = Nothing
--        , collectionReplicationFactor = Nothing
--        })
createCollection
  :: CreateCollectionRequest
  -> ArangoClientM (CollectionInfo `WithFields` CollectionProperties)
createCollection = arangoClient (Proxy @CreateCollection)

-- | Create collection request parameters.
--
-- Note that you can use `OverloadedStrings` to only specify
-- 'CollectionName' and leave everything else as 'Nothing'.
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

-- | Drop a regular (non-system) collection.
-- To drop system collections see 'dropSystemCollection'.
--
-- >>> _ <- runDefault $ createCollection "hs-drop-example"
-- >>> runDefault $ dropCollection "hs-drop-example"
-- Right "..."
dropCollection :: CollectionName -> ArangoClientM CollectionId
dropCollection = flip dropCollectionClient Nothing

-- | Drop a system collection.
-- To drop regular (non-system) collections see 'dropCollection'.
dropSystemCollection :: CollectionName -> ArangoClientM CollectionId
dropSystemCollection = flip dropCollectionClient (Just True)

dropCollectionClient
  :: CollectionName
  -> Maybe Bool      -- ^ Is this a system collection?
  -> ArangoClientM CollectionId
dropCollectionClient name isSystem = unOnlyField
  <$> arangoClient (Proxy @DropCollection) name isSystem

-- ** Truncate a collection

type TruncateCollection
  = "collection"
 :> Capture "collection-name" CollectionName
 :> "truncate"
 :> Put '[JSON] CollectionInfo

-- | Truncate a collection (delete all documents).
--
-- >>> runDefaultPretty $ truncateCollection "hs-example"
-- Right
--   CollectionInfo
--     { collectionId = "..."
--     , collectionName = "hs-example"
--     , collectionStatus = CollectionLoaded
--     , collectionType = DocumentCollection
--     , collectionIsSystem = False
--     }
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

-- | Get basic 'CollectionInfo'.
--
-- >>> runDefaultPretty $ getCollection "hs-example"
-- Right
--   CollectionInfo
--     { collectionId = "..."
--     , collectionName = "hs-example"
--     , collectionStatus = CollectionLoaded
--     , collectionType = DocumentCollection
--     , collectionIsSystem = False
--     }
getCollection :: CollectionName -> ArangoClientM CollectionInfo
getCollection = arangoClient (Proxy @GetCollection)

-- ** Read properties of a collection

type GetCollectionProperties
  = "collection"
 :> Capture "collection-name" CollectionName
 :> "properties"
 :> Get '[JSON] (CollectionInfo `WithFields` CollectionProperties)

-- | Get collection info together with collection properties.
--
-- >>> runDefaultPretty $ getCollectionProperties "hs-example"
-- Right
--   (WithFields
--      CollectionInfo
--        { collectionId = "..."
--        , collectionName = "hs-example"
--        , collectionStatus = CollectionLoaded
--        , collectionType = DocumentCollection
--        , collectionIsSystem = False
--        }
--      CollectionProperties
--        { collectionWaitForSync = False
--        , collectionDoCompact = True
--        , collectionJournalSize = ...
--        , collectionKeyOptions =
--            CollectionKeyOptions
--              { collectionKeyType = KeyGeneratorTraditional
--              , collectionKeyAllowUserKeys = True
--              }
--        , collectionIsVolatile = False
--        , collectionNumberOfShards = Nothing
--        , collectionShardKeys = Nothing
--        , collectionReplicationFactor = Nothing
--        })
getCollectionProperties
  :: CollectionName -> ArangoClientM (CollectionInfo `WithFields` CollectionProperties)
getCollectionProperties = arangoClient (Proxy @GetCollectionProperties)

-- | Like 'getCollectionProperties' but returns just 'CollectionProperties'.
--
-- >>> runDefaultPretty $ getCollectionProperties' "hs-example"
-- Right
--   CollectionProperties
--     { collectionWaitForSync = False
--     , collectionDoCompact = True
--     , collectionJournalSize = ...
--     , collectionKeyOptions =
--         CollectionKeyOptions
--           { collectionKeyType = KeyGeneratorTraditional
--           , collectionKeyAllowUserKeys = True
--           }
--     , collectionIsVolatile = False
--     , collectionNumberOfShards = Nothing
--     , collectionShardKeys = Nothing
--     , collectionReplicationFactor = Nothing
--     }
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

-- | Retrieve collection info, properties and document count.
--
-- >>> runDefaultPretty $ getCollectionCount "hs-example"
-- Right
--   (WithFields
--      (WithFields
--         OnlyField { unOnlyField = 0 }
--         CollectionInfo
--           { collectionId = "..."
--           , collectionName = "hs-example"
--           , collectionStatus = CollectionLoaded
--           , collectionType = DocumentCollection
--           , collectionIsSystem = False
--           })
--      CollectionProperties
--        { collectionWaitForSync = False
--        , collectionDoCompact = True
--        , collectionJournalSize = ...
--        , collectionKeyOptions =
--            CollectionKeyOptions
--              { collectionKeyType = KeyGeneratorTraditional
--              , collectionKeyAllowUserKeys = True
--              }
--        , collectionIsVolatile = False
--        , collectionNumberOfShards = Nothing
--        , collectionShardKeys = Nothing
--        , collectionReplicationFactor = Nothing
--        })
getCollectionCount
  :: CollectionName -> ArangoClientM (OnlyField "count" Integer `WithFields` CollectionInfo `WithFields` CollectionProperties)
getCollectionCount = arangoClient (Proxy @GetCollectionCount)

-- | Like 'getCollectionCount' but returns just the count.
--
-- >>> runDefaultPretty $ getCollectionCount' "hs-example"
-- Right 0
getCollectionCount' :: CollectionName -> ArangoClientM Integer
getCollectionCount' name = extractCount <$> getCollectionCount name
  where
    extractCount (WithFields (WithFields (OnlyField n) _) _) = n

-- ** Read all collections

type GetAllCollections
  = "collection"
 :> QueryParam "excludeSystem" Bool
 :> Get '[JSON] (OnlyField "result" [CollectionInfo])

-- | Get all collections (including system collections).
getAllCollections :: ArangoClientM [CollectionInfo]
getAllCollections = getAllCollectionsClient Nothing

-- | Get all non-system collections.
--
-- >>> runDefaultPretty $ getAllNonSystemCollections
-- Right
--   [ CollectionInfo
--       { collectionId = "..."
--       , collectionName = "hs-example"
--       , collectionStatus = CollectionLoaded
--       , collectionType = DocumentCollection
--       , collectionIsSystem = False
--       }
--   ]
getAllNonSystemCollections :: ArangoClientM [CollectionInfo]
getAllNonSystemCollections = getAllCollectionsClient (Just True)

getAllCollectionsClient
  :: Maybe Bool   -- ^ Exclude system collections?
  -> ArangoClientM [CollectionInfo]
getAllCollectionsClient excludeSystem
  = unOnlyField <$> arangoClient (Proxy @GetAllCollections) excludeSystem

-- * Types

newtype CollectionId = CollectionId Text
  deriving newtype (IsString, Eq, Show, ToJSON, FromJSON)

newtype CollectionName = CollectionName Text
  deriving newtype (IsString, Eq, Show, ToHttpApiData, ToJSON, FromJSON)

data CollectionStatus
  = CollectionNewBorn
  | CollectionUnloaded
  | CollectionLoaded
  | CollectionUnloading
  | CollectionDeleted
  | CollectionLoading
  deriving (GHC.Generic, Generic, Eq, Show, Bounded)

-- |
-- >>> map toEnum [1..6] :: [CollectionStatus]
-- [CollectionNewBorn,CollectionUnloaded,CollectionLoaded,CollectionUnloading,CollectionDeleted,CollectionLoading]
--
-- >>> map fromEnum [CollectionNewBorn .. CollectionLoading]
-- [1,2,3,4,5,6]
instance Enum CollectionStatus where
  fromEnum = (+ 1) . gfromEnum
  toEnum = gtoEnum . (subtract 1)

-- |
-- >>> decode "[1, 2, 3, 4, 5, 6]" :: Maybe [CollectionStatus]
-- Just [CollectionNewBorn,CollectionUnloaded,CollectionLoaded,CollectionUnloading,CollectionDeleted,CollectionLoading]
--
-- >>> eitherDecode "0" :: Either String CollectionStatus
-- Left "Error in $: illegal collection status: 0 (expected a value between 1 and 6)"
instance FromJSON CollectionStatus where
  parseJSON = parseJSONBoundedEnum "collection status"

-- |
-- >>> encode [CollectionNewBorn .. CollectionLoading]
-- "[1,2,3,4,5,6]"
instance ToJSON CollectionStatus where
  toJSON = toJSONEnum

data CollectionType
  = DocumentCollection
  | EdgeCollection
  deriving (GHC.Generic, Generic, Eq, Show, Bounded)

-- |
-- >>> map toEnum [2, 3] :: [CollectionType]
-- [DocumentCollection,EdgeCollection]
--
-- >>> map fromEnum [DocumentCollection, EdgeCollection]
-- [2,3]
instance Enum CollectionType where
  fromEnum = (+2) . gfromEnum
  toEnum   = gtoEnum . (subtract 2)

-- |
-- >>> decode "[2, 3]" :: Maybe [CollectionType]
-- Just [DocumentCollection,EdgeCollection]
--
-- >>> eitherDecode "4" :: Either String CollectionType
-- Left "Error in $: illegal collection type: 4 (expected a value between 2 and 3)"
instance FromJSON CollectionType where
  parseJSON = parseJSONBoundedEnum "collection type"

-- |
-- >>> encode [DocumentCollection, EdgeCollection]
-- "[2,3]"
instance ToJSON CollectionType where
  toJSON = toJSONEnum

data CollectionInfo = CollectionInfo
  { collectionId       :: CollectionId
  , collectionName     :: CollectionName
  , collectionStatus   :: CollectionStatus
  , collectionType     :: CollectionType
  , collectionIsSystem :: Bool
  } deriving (Eq, Show)

data KeyGeneratorType
  = KeyGeneratorTraditional
  | KeyGeneratorAutoincrement
  deriving (Eq, Show)

data CollectionKeyOptions = CollectionKeyOptions
  { collectionKeyType          :: KeyGeneratorType
  , collectionKeyAllowUserKeys :: Bool
  } deriving (Eq, Show)

newtype DocumentAttrName = DocumentAttrName Text
  deriving newtype (IsString, Eq, Show, ToJSON, FromJSON)

data CollectionProperties = CollectionProperties
  { collectionWaitForSync       :: Bool
  , collectionDoCompact         :: Bool
  , collectionJournalSize       :: Int
  , collectionKeyOptions        :: CollectionKeyOptions
  , collectionIsVolatile        :: Bool
  , collectionNumberOfShards    :: Maybe Int
  , collectionShardKeys         :: Maybe [DocumentAttrName]
  , collectionReplicationFactor :: Maybe Int
  } deriving (Eq, Show)

-- Template Haskell derivations
deriveJSON' ''CollectionInfo
deriveJSON' ''KeyGeneratorType
deriveJSON' ''CollectionKeyOptions
deriveJSON' ''CollectionProperties
deriveJSON' ''CreateCollectionRequest
