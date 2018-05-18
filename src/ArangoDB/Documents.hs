{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- See <https://docs.arangodb.com/3.3/HTTP/Document/>.
module ArangoDB.Documents where

import           ArangoDB.Types
import           ArangoDB.Utils.Client
import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Proxy
import           Servant.API

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XFlexibleInstances
-- >>> :set -XDeriveGeneric
-- >>> :set -XScopedTypeVariables
-- >>> :set -XDeriveAnyClass
-- >>> import Data.Aeson
-- >>> import GHC.Generics
-- >>> import Data.Either (isRight, isLeft)
-- >>> import ArangoDB.Collections
-- >>> data Person = Person { firstname :: String, lastname :: String } deriving (Show, Generic, ToJSON, FromJSON)
-- >>> user = Person "Nick" "K."
-- >>> collectionName = "Person"
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
 :> Capture "collection-name" (TypedCollectionName a)
 :> QueryParam "returnNew" Bool
 :> QueryParam "waitForSync" Bool
 :> QueryParam "silent" Bool
 :> ReqBody '[JSON] a
 :> Post '[JSON] CreateDocumentResponse

createDocument :: forall a. (ToJSON a, FromJSON a) =>
            (TypedCollectionName a)
            -> ReturnNew
            -> WaitForSync
            -> Silent
            -> a
            -> ArangoClientM CreateDocumentResponse
createDocument = arangoClient (Proxy @(CreateDocument a))


type GetDocument a
  = "document"
 :> Capture "collection-name" (TypedCollectionName a)
 :> Capture "document-key" DocumentKey
 :> Get '[JSON] (Document a)

getDocument :: forall a. FromJSON a => (TypedCollectionName a) -> DocumentKey -> ArangoClientM (Document a)
getDocument = arangoClient (Proxy @(GetDocument a))

getDocumentById :: FromJSON a => DocumentId a -> ArangoClientM (Document a)
getDocumentById = uncurry getDocument . splitDocumentId

type DropDocument a
  = "document"
 :> Capture "collection-name" (TypedCollectionName a)
 :> QueryParam "returnOld" Bool
 :> Capture "document-key" DocumentKey
 :> QueryParam "waitForSync" Bool
 :> QueryParam "silent" Bool
 :> Header "If-Match" DocumentRevision
 :> ReqBody '[JSON] a
 :> Delete '[JSON] DropDocumentResponse


dropDocument :: forall a. (ToJSON a, FromJSON a) =>
               (TypedCollectionName a)
               -> ReturnOld
               -> DocumentKey
               -> WaitForSync
               -> Silent
               -> IfMatch
               -> a
               -> ArangoClientM DropDocumentResponse
dropDocument = arangoClient (Proxy @(DropDocument a))

type DropDocumentFull a
  = "document"
 :> Capture "collection-name" (TypedCollectionName a)
 :> QueryParam "returnOld" Bool
 :> Capture "document-key" DocumentKey
 :> QueryParam "waitForSync" Bool
 :> QueryParam "silent" Bool
 :> Header "If-Match" DocumentRevision
 :> ReqBody '[JSON] a
 :> Delete '[JSON] (DropDocumentResponseFull a)


dropDocumentFull :: forall a. (ToJSON a, FromJSON a) =>
               (TypedCollectionName a)
               -> DocumentKey
               -> WaitForSync
               -> Silent
               -> IfMatch
               -> a
               -> ArangoClientM (DropDocumentResponseFull a)
dropDocumentFull typedCollectionName docKey waitForSync silent ifMatch doc = arangoClient (Proxy @(DropDocumentFull a)) typedCollectionName (Just True) docKey waitForSync silent ifMatch doc

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