{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module ArangoDB.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict as HashMap
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Generics.Deriving.Eq (GEq)
import Generics.Deriving.Enum (fromEnumDefault, toEnumDefault)
import GHC.Generics (Generic)
import Web.HttpApiData (ToHttpApiData)

import ArangoDB.Utils.Aeson

-- * Collections

newtype CollectionId = CollectionId Text
  deriving newtype (IsString, Show, ToJSON, FromJSON)

newtype CollectionName = CollectionName Text
  deriving newtype (IsString, Show, ToHttpApiData, ToJSON, FromJSON)

data CollectionStatus
  = CollectionNewBorn
  | CollectionUnloaded
  | CollectionLoaded
  | CollectionUnloading
  | CollectionDeleted
  | CollectionLoading
  deriving (Generic, GEq, Show, Bounded)

-- |
-- >>> map toEnum [1..6] :: [CollectionStatus]
-- [CollectionNewBorn,CollectionUnloading,CollectionUnloaded,CollectionDeleted,CollectionLoaded,CollectionLoading]
--
-- >>> map fromEnum [CollectionNewBorn .. CollectionLoading]
-- [1,2,3,4,5,6]
instance Enum CollectionStatus where
  fromEnum = succ . fromEnumDefault
  toEnum = toEnumDefault . pred

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
  deriving (Generic, GEq, Show, Bounded)

-- |
-- >>> map toEnum [2, 3] :: [CollectionType]
-- [DocumentCollection,EdgeCollection]
--
-- >>> map fromEnum [DocumentCollection, EdgeCollection]
-- [2,3]
instance Enum CollectionType where
  fromEnum = (+2) . fromEnumDefault
  toEnum   = toEnumDefault . (subtract 2)

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
  } deriving (Show)

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

instance ToJSON a => ToJSON (Document a) where
  toJSON Document{..} = Object $ case toJSON documentValue of
    Object o -> HashMap.union specialFields o
    value -> HashMap.insert "value" value specialFields
    where
      specialFields = HashMap.fromList
        [ "_id"  .= documentId
        , "_key" .= documentKey
        , "_rev" .= documentRev
        ]

instance FromJSON a => FromJSON (Document a) where
  parseJSON js = flip (withObject "Document") js $ \o -> Document
    <$> o .: "_id"
    <*> o .: "_key"
    <*> o .: "_rev"
    <*> (parseJSON js <|> o .: "value")

-- Template Haskell derivations
deriveJSON' ''CollectionInfo
