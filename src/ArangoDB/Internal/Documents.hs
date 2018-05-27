{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
-- |
-- See <https://docs.arangodb.com/3.3/HTTP/Document/>.
module ArangoDB.Internal.Documents where

import           ArangoDB.Utils.API
import           ArangoDB.Utils.Client

import           Control.Applicative   ((<|>))
import           Control.Monad         (void)
import           Data.Aeson
import           Data.Aeson.Unit
import           Data.Aeson.WithField  (OnlyField (..))
import qualified Data.HashMap.Strict   as HashMap
import           Data.Monoid
import           Data.Proxy
import           Data.String           (IsString)
import           Data.Text             (Text)
import qualified Data.Text             as Text

import           Servant.API

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XFlexibleInstances
-- >>> :set -XDeriveGeneric
-- >>> :set -XScopedTypeVariables
-- >>> :set -XDeriveAnyClass
--
-- >>> import Data.Aeson
-- >>> import GHC.Generics
-- >>> import Data.Either (isRight, isLeft)
-- >>> import ArangoDB.Collections
--
-- >>> data Person = Person { firstname :: String, lastname :: String } deriving (Show, Generic, ToJSON, FromJSON)
-- >>> user = Person "Nick" "K."
-- >>> collectionName = "Person"
--
-- >>> createCollectionResult = runDefault $ createCollection collectionName
-- >>> fmap isRight createCollectionResult
-- True

type WaitForSync = Maybe Bool
type ReturnOld   = Maybe Bool
type Silent      = Maybe Bool
type IfMatch     = Maybe DocumentRevision
type ReturnNew   = Maybe Bool

type CreateDocument a
  = "document"
 :> QueryParam "returnNew" Bool
 :> QueryParam "waitForSync" Bool
 :> QueryParam "silent" Bool
 :> Capture "collection-name" (TypedCollectionName a)
 :> ReqBody '[JSON] a
 :> Post '[JSON] (Document Unit)

-- | Create a new document in a given collection.
createDocument :: (ToJSON a, FromJSON a) =>
  TypedCollectionName a -> a -> ArangoClientM (Document Unit)
createDocument = createDocument' Nothing Nothing Nothing

-- | Like 'createDocument' but with extra options.
createDocument'
  :: forall a. (ToJSON a, FromJSON a)
  => ReturnNew
  -> WaitForSync
  -> Silent
  -> TypedCollectionName a
  -> a
  -> ArangoClientM (Document Unit)
createDocument' = arangoClient (Proxy @(CreateDocument a))

-- * Read documents

type GetDocument a
  = "document"
 :> Capture "collection-name" (TypedCollectionName a)
 :> Capture "document-key" DocumentKey
 :> Get '[JSON] (Document a)

-- | Retrieve a document by its key from a given collection.
getDocument
  :: forall a. FromJSON a
  => TypedCollectionName a
  -> DocumentKey
  -> ArangoClientM (Document a)
getDocument = arangoClient (Proxy @(GetDocument a))

-- | Retrieve a document by its full 'DocumentId'
-- (which includes collection name).
getDocumentById :: FromJSON a => DocumentId a -> ArangoClientM (Document a)
getDocumentById = uncurry getDocument . splitDocumentId

-- * Remove documents

-- | Drop document endpoint and common parameters.
type DropDocument' a api
  = "document"
 :> Capture "collection-name" (TypedCollectionName a)
 :> Capture "document-key" DocumentKey
 :> Header "If-Match" DocumentRevision
 :> QueryParam "waitForSync" Bool
 :> api

-- | Regular drop document endpoint.
type DropDocument a = DropDocument' a
  (Delete '[JSON] (Document Unit))

-- | Drop document endpoint that returns removed document.
type DropDocumentReturnOld a = DropDocument' a
  ( QueryParam "returnOld" Bool :==: 'Just 'True
 :> Delete '[JSON] (Document (OnlyField "old" a)) )

-- | Drop document endpoint that returns no content.
type DropDocumentSilent a = DropDocument' a
  ( QueryParam "silent" Bool :==: 'Just 'True
 :> Delete '[JSON] NoContent )

-- | Regular drop document endpoint with extra parameters.
dropDocument' :: forall a. TypedCollectionName a -> DocumentKey -> IfMatch -> WaitForSync -> ArangoClientM (Document Unit)
dropDocument' = arangoClient (Proxy @(DropDocument a))

-- | Drop document endpoint with extra parameters, returning removed document.
dropDocumentReturnOld' :: forall a. FromJSON a => TypedCollectionName a -> DocumentKey -> IfMatch -> WaitForSync -> ArangoClientM (Document (OnlyField "old" a))
dropDocumentReturnOld' = arangoClient (Proxy @(DropDocumentReturnOld a))

-- | Drop document endpoint with extra parameters, returning no content.
dropDocumentSilent' :: forall a. TypedCollectionName a -> DocumentKey -> IfMatch -> WaitForSync -> ArangoClientM NoContent
dropDocumentSilent' = arangoClient (Proxy @(DropDocumentSilent a))

-- | Remove a document from collection, returning old document info.
dropDocument :: TypedCollectionName a -> DocumentKey -> ArangoClientM (Document Unit)
dropDocument n k = dropDocument' n k Nothing Nothing

-- | Like 'dropDocument', but does not retrieve old document info.
dropDocument_ :: TypedCollectionName a -> DocumentKey -> ArangoClientM ()
dropDocument_ n k = void (dropDocumentSilent' n k Nothing Nothing)

-- * Update document

type UpdateDocument' a api
  = "document"
  :> Capture "collection-name" (TypedCollectionName a)
  :> Capture "document-key" DocumentKey
  :> ReqBody '[JSON] a
  :> Header "If-Match" DocumentRevision
  :> QueryParam "waitForSync" Bool
  :> QueryParam "keepNull" Bool
  :> QueryParam "mergeObjects" Bool
  :> api

type UpdateDocument a = UpdateDocument' a
  ( Put '[JSON] (Document Unit) )

type UpdateDocumentReturnOld a = UpdateDocument' a
  ( QueryParam "returnOld" Bool :==: 'Just 'True
 :> Put '[JSON] (Document (OnlyField "old" (Document a))) )

type UpdateDocumentSilent a = UpdateDocument' a
  ( QueryParam "silent" Bool :==: 'Just 'True
 :> Put '[JSON] NoContent )

updateDocument'
  :: forall a. ToJSON a
  => TypedCollectionName a
  -> DocumentKey
  -> a
  -> IfMatch
  -> WaitForSync
  -> Maybe Bool -- keepNull
  -> Maybe Bool -- mergeObjects
  -> ArangoClientM (Document Unit)
updateDocument' = arangoClient (Proxy @(UpdateDocument a))

updateDocumentReturnOld'
  :: forall a. (ToJSON a, FromJSON a)
  => TypedCollectionName a
  -> DocumentKey
  -> a
  -> IfMatch
  -> WaitForSync
  -> Maybe Bool -- keepNull
  -> Maybe Bool -- mergeObjects
  -> ArangoClientM (Document (OnlyField "old" (Document a)))
updateDocumentReturnOld' = arangoClient (Proxy @(UpdateDocumentReturnOld a))

updateDocumentSilent'
  :: forall a. ToJSON a
  => TypedCollectionName a
  -> DocumentKey
  -> a
  -> IfMatch
  -> WaitForSync
  -> Maybe Bool -- keepNull
  -> Maybe Bool -- mergeObjects
  -> ArangoClientM NoContent
updateDocumentSilent' = arangoClient (Proxy @(UpdateDocumentSilent a))

-- | Update a document with default parameters,
-- returning document info for the updated document.
updateDocument
  :: forall a. ToJSON a
  => TypedCollectionName a
  -> DocumentKey
  -> a
  -> ArangoClientM (Document Unit)
updateDocument n k x = updateDocument' n k x Nothing Nothing Nothing Nothing

-- | Update a document with default parameters, returning no content.
updateDocument_
  :: forall a. ToJSON a
  => TypedCollectionName a
  -> DocumentKey
  -> a
  -> ArangoClientM ()
updateDocument_ n k x = void $
  updateDocumentSilent' n k x Nothing Nothing Nothing Nothing

-- * Documents

newtype DocumentId a = DocumentId Text
  deriving newtype (Show, ToJSON, FromJSON)

mkDocumentId :: (TypedCollectionName a) -> DocumentKey -> DocumentId a
mkDocumentId (TypedCollectionName name) (DocumentKey key)
  = DocumentId (name <> "/" <> key)

newtype DocumentKey = DocumentKey Text
  deriving newtype (IsString, Show, ToJSON, FromJSON, ToHttpApiData)

newtype DocumentRevision = DocumentRevision Text
  deriving newtype (Show, ToJSON, FromJSON, ToHttpApiData)

data Document a = Document
  { documentId    :: DocumentId a
  , documentKey   :: DocumentKey
  , documentRev   :: DocumentRevision
  , documentValue :: a
  } deriving (Show)

instance ToJSON a => ToJSON (Document a) where
  toJSON Document{..} = Object $ case toJSON documentValue of
    Object o -> HashMap.union specialFields o
    value    -> HashMap.insert "value" value specialFields
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

-- * Document

newtype TypedCollectionName a = TypedCollectionName Text deriving newtype (IsString, Eq, Show, ToHttpApiData, ToJSON, FromJSON)

splitDocumentId :: DocumentId a -> (TypedCollectionName a, DocumentKey)
splitDocumentId (DocumentId handle) = case Text.splitOn "/" handle of
  [name, key] -> (TypedCollectionName name, DocumentKey key)
  _ -> error ("Impossible happened! Invalid document id: " ++ show handle)
