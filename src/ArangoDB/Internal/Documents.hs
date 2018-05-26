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
 :> Post '[JSON] CreateDocumentResponse

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
  -> ArangoClientM CreateDocumentResponse
createDocument' = arangoClient (Proxy @(CreateDocument a))

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

-- * Removing documents

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
 :> Delete '[JSON] (Document a) )

-- | Drop document endpoint that returns no content.
type DropDocumentSilent a = DropDocument' a
  ( QueryParam "silent" Bool :==: 'Just 'True
 :> Delete '[JSON] NoContent )

-- | Regular drop document endpoint with extra parameters.
dropDocument' :: forall a. TypedCollectionName a -> DocumentKey -> IfMatch -> WaitForSync -> ArangoClientM (Document Unit)
dropDocument' = arangoClient (Proxy @(DropDocument a))

-- | Drop document endpoint with extra parameters, returning removed document.
dropDocumentReturnOld' :: forall a. FromJSON a => TypedCollectionName a -> DocumentKey -> IfMatch -> WaitForSync -> ArangoClientM (Document a)
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

type UpdateDocument a
    = "document"
    :> Capture "collection-name" (TypedCollectionName a)
    :> QueryParam "returnOld" Bool
    :> Capture "document-key" DocumentKey
    :> QueryParam "waitForSync" Bool
    :> QueryParam "silent" Bool
    :> Header "If-Match" DocumentRevision
    :> ReqBody '[JSON] a
    :> Put '[JSON] UpdateDocumentResponse

updateDocument :: forall a. (ToJSON a, FromJSON a) =>
            (TypedCollectionName a)
            -> ReturnOld
            -> DocumentKey
            -> WaitForSync
            -> Silent
            -> IfMatch
            -> a
            -> ArangoClientM UpdateDocumentResponse
updateDocument = arangoClient (Proxy @(UpdateDocument a))

type UpdateDocumentFull a
    = "document"
    :> Capture "collection-name" (TypedCollectionName a)
    :> QueryParam "returnOld" Bool
    :> Capture "document-key" DocumentKey
    :> QueryParam "waitForSync" Bool
    :> QueryParam "silent" Bool
    :> Header "If-Match" DocumentRevision
    :> ReqBody '[JSON] a
    :> Put '[JSON] (UpdateDocumentResponseFull a)

updateDocumentFull :: forall a. (ToJSON a, FromJSON a) =>
            (TypedCollectionName a)
            -> DocumentKey
            -> WaitForSync
            -> Silent
            -> IfMatch
            -> a
            -> ArangoClientM (UpdateDocumentResponseFull a)
updateDocumentFull typedCollectionName docKey waitForSync silent ifMatch doc = arangoClient (Proxy @(UpdateDocumentFull a)) typedCollectionName (Just True) docKey waitForSync silent ifMatch doc

-- | Doctest for the functions above
-- >>> createDocumentResult = runDefault $ createDocument (TypedCollectionName collectionName) (Just False) (Just False) (Just False) user
-- >>> fmap isRight createDocumentResult
-- True
-- >>> Right (Document docId docKey docRev _) <- createDocumentResult
-- >>> Right (person :: Document Person) <- runDefault $ getDocument (TypedCollectionName collectionName) docKey
-- >>> documentValue person
-- Person {firstname = "Nick", lastname = "K."}
-- >>> failedDropResult = runDefault $ dropDocument (TypedCollectionName collectionName) (Just False)  (DocumentKey "key")  (Just False) (Just False) (Just (DocumentRevision "1")) user
-- >>> fmap isLeft failedDropResult
-- True
-- >>> dropResult = runDefault $ dropDocument (TypedCollectionName collectionName) (Just False)  docKey  (Just False) (Just False) (Just docRev) user
-- >>> fmap isRight dropResult
-- True
-- >>> user1 = Person "gang" "w."
-- >>> createDocumentResult = runDefault $ createDocument (TypedCollectionName collectionName) (Just False) (Just False) (Just False) user
-- >>> Right (Document docId docKey docRev _) <- createDocumentResult
-- >>> updateResult = runDefault $ updateDocument (TypedCollectionName collectionName) (Just False)  docKey  (Just False) (Just False) (Just docRev) user1
-- >>> fmap isRight updateResult
-- True
-- >>> dropCollectionResult = runDefault $ dropCollection collectionName
-- >>> fmap isRight dropCollectionResult
-- True

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


type DeleteDocumentResponse       = Document (OnlyField "old" (Maybe DocumentRevision))
type CreateDocumentResponse       = Document Unit
type UpdateDocumentResponse       = Document Unit
type UpdateDocumentResponseFull a = Document (OnlyField "old" a)
type DropDocumentResponse         = Document Unit
type DropDocumentResponseFull   a = Document (OnlyField "old" a)

-- * Document

newtype TypedCollectionName a = TypedCollectionName Text deriving newtype (IsString, Eq, Show, ToHttpApiData, ToJSON, FromJSON)


splitDocumentId :: DocumentId a -> (TypedCollectionName a, DocumentKey)
splitDocumentId (DocumentId handle) = case Text.splitOn "/" handle of
  [name, key] -> (TypedCollectionName name, DocumentKey key)
  _ -> error ("Impossible happened! Invalid document id: " ++ show handle)
