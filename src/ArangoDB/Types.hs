{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module ArangoDB.Types where

import Data.Aeson
import Data.String (IsString)
import Data.Text (Text)
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

-- Template Haskell derivations
deriveJSON' ''Collection
