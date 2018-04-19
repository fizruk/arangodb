{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}   -- needed for TypeError constaints
module ArangoDB.Types where

import Data.Aeson
import Data.Aeson.Unit
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Generics.SOP (Generic)
import GHC.TypeLits
import qualified GHC.Generics as GHC
import Web.HttpApiData (ToHttpApiData)

import ArangoDB.Utils.Aeson
import ArangoDB.Utils.Enum

-- * Collections

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

-- * Documents

newtype DocumentId = DocumentId Text
  deriving newtype (Show, ToJSON, FromJSON)

mkDocumentId :: CollectionName -> DocumentKey -> DocumentId
mkDocumentId (CollectionName name) (DocumentKey key)
  = DocumentId (name <> "/" <> key)

splitDocumentId :: DocumentId -> (CollectionName, DocumentKey)
splitDocumentId (DocumentId handle) = case Text.splitOn "/" handle of
  [name, key] -> (CollectionName name, DocumentKey key)
  _ -> error ("Impossible happened! Invalid document id: " ++ show handle)

newtype DocumentKey = DocumentKey Text
  deriving newtype (IsString, Show, ToJSON, FromJSON, ToHttpApiData)

newtype DocumentRevision = DocumentRevision Text
  deriving newtype (Show, ToJSON, FromJSON, ToHttpApiData)

data Document a = Document
  { documentId    :: DocumentId
  , documentKey   :: DocumentKey
  , documentRev   :: DocumentRevision
  , documentValue :: a
  } deriving (Show)

-- | A value of type @a@ can be encoded as an ArangoDB 'Document'
-- only when its JSON encoding is an 'Object'.
-- 
-- See 'unsafeToJSONObject'.
instance ToJSON a => ToJSON (Document a) where
  toJSON Document{..} = Object
    (HashMap.union specialFields (unsafeToJSONObject documentValue))
    where
      specialFields = HashMap.fromList
        [ "_id"  .= documentId
        , "_key" .= documentKey
        , "_rev" .= documentRev
        ]

class HasObjectEncoding a
instance {-# OVERLAPPING #-} TypeError ('Text "error") => HasObjectEncoding ()
instance {-# OVERLAPPING #-} TypeError ('Text "error") => HasObjectEncoding Integer
instance {-# OVERLAPPING #-} TypeError ('Text "error") => HasObjectEncoding Double
instance {-# OVERLAPPING #-} TypeError ('Text "error") => HasObjectEncoding String
instance FromJSON a => HasObjectEncoding a

instance (FromJSON a, HasObjectEncoding a) => FromJSON (Document a) where
  parseJSON js = flip (withObject "Document") js $ \o -> Document
    <$> o .: "_id"
    <*> o .: "_key"
    <*> o .: "_rev"
    <*> parseJSON js

type DropDocumentResponse = Document Unit

type CreateDocumentResponse = Document Unit

-- Template Haskell derivations
deriveJSON' ''CollectionInfo
deriveJSON' ''KeyGeneratorType
deriveJSON' ''CollectionKeyOptions
deriveJSON' ''CollectionProperties
