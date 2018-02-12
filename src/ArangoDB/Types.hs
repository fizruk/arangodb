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
import Web.HttpApiData (ToHttpApiData)
import ArangoDB.Utils.Aeson (deriveJSON')

-- * Collections

newtype CollectionId = CollectionId Text
  deriving (IsString, Show, ToJSON, FromJSON)

newtype CollectionName = CollectionName Text
  deriving (IsString, Show, ToHttpApiData, ToJSON, FromJSON)

data CollectionStatus
  = CollectionNewBorn
  | CollectionUnloaded
  | CollectionLoaded
  | CollectionUnloading
  | CollectionDeleted
  | CollectionLoading
  deriving (Show, Enum)

-- |
-- >>> decode "[1, 2, 3, 4, 5, 6]" :: Maybe [CollectionStatus]
-- Just [CollectionNewBorn,CollectionUnloaded,CollectionLoaded,CollectionUnloading,CollectionDeleted,CollectionLoading]
instance FromJSON CollectionStatus where
  parseJSON js = (toEnum . pred) <$> parseJSON js

-- |
-- >>> encode [CollectionNewBorn .. CollectionLoading]
-- "[1,2,3,4,5,6]"
instance ToJSON CollectionStatus where
  toJSON = toJSON . succ . fromEnum

data Collection = Collection
  { collectionId     :: CollectionId
  , collectionName   :: CollectionName
  , collectionStatus :: CollectionStatus
  } deriving (Show)

-- * Documents

newtype DocumentId = DocumentId Text
  deriving (Show, ToJSON, FromJSON)

mkDocumentId :: CollectionName -> DocumentKey -> DocumentId
mkDocumentId (CollectionName name) (DocumentKey key)
  = DocumentId (name <> "/" <> key)

splitDocumentId :: DocumentId -> (CollectionName, DocumentKey)
splitDocumentId (DocumentId handle) = case Text.splitOn "/" handle of
  [name, key] -> (CollectionName name, DocumentKey key)
  _ -> error ("Impossible happened! Invalid document id: " ++ show handle)

newtype DocumentKey = DocumentKey Text
  deriving (IsString, Show, ToJSON, FromJSON, ToHttpApiData)

newtype DocumentRevision = DocumentRevision Text
  deriving (Show, ToJSON, FromJSON, ToHttpApiData)

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
deriveJSON' ''Collection
