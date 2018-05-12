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
-- >>> :set -XDeriveAnyClass
-- >>> :set -XScopedTypeVariables
-- >>> import Data.Aeson
-- >>> import GHC.Generics
-- >>> import Data.Either (isRight, isLeft)
-- >>> import ArangoDB.Collections
-- >>> data Person = Person { firstname :: String, lastname :: String } deriving (Show, Generic, ToJSON, FromJSON)
-- >>> user = Person "Nick" "K."
-- >>> collectionName = "example"
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

getDocumentById :: FromJSON a => DocumentId -> ArangoClientM (Document a)
getDocumentById = uncurry getDocument . splitDocumentId

type DropDocument
  = "document"
 :> Capture "collection-name" CollectionName
 :> QueryParam "returnOld" Bool
 :> Capture "document-key" DocumentKey
 :> QueryParam "waitForSync" Bool
 :> QueryParam "silent" Bool
 :> Header "If-Match" DocumentRevision
 :> Delete '[JSON] DropDocumentResponse


dropDocument :: CollectionName
               -> ReturnOld
               -> DocumentKey
               -> WaitForSync
               -> Silent
               -> IfMatch
               -> ArangoClientM DropDocumentResponse
dropDocument = arangoClient (Proxy @DropDocument)

type UpdateDocument a
    = "document"
    :> Capture "collection-name" CollectionName
    :> QueryParam "returnOld" Bool
    :> Capture "document-key" DocumentKey
    :> QueryParam "waitForSync" Bool
    :> QueryParam "silent" Bool
    :> Header "If-Match" DocumentRevision
    :> ReqBody '[JSON] a
    :> Put '[JSON] UpdateDocumentResponse

updateDocument :: forall a. (ToJSON a, FromJSON a) =>
            CollectionName
            -> ReturnOld
            -> DocumentKey
            -> WaitForSync
            -> Silent
            -> IfMatch
            -> a
            -> ArangoClientM UpdateDocumentResponse
updateDocument = arangoClient (Proxy @(UpdateDocument a))

-- | Doctest for the functions above
-- >>> createDocumentResult = runDefault $ createDocument collectionName (Just False) (Just False) (Just False) user
-- >>> fmap isRight createDocumentResult
-- True
-- >>> Right (Document docId docKey docRev _) <- createDocumentResult
-- >>> Right (person :: Document Person) <- runDefault $ getDocument collectionName docKey
-- >>> documentValue person
-- Person {firstname = "Nick", lastname = "K."}
-- >>> failedDropResult = runDefault $ dropDocument "example" (Just False)  (DocumentKey "key")  (Just False) (Just False) (Just (DocumentRevision "1"))
-- >>> fmap isLeft failedDropResult
-- True
-- >>> dropResult = runDefault $ dropDocument collectionName (Just False)  docKey  (Just False) (Just False) (Just docRev)
-- >>> fmap isRight dropResult
-- True
-- >>> user1 = Person "gang" "w."
-- >>> createDocumentResult = runDefault $ createDocument collectionName (Just False) (Just False) (Just False) user
-- >>> Right (Document docId docKey docRev _) <- createDocumentResult
-- >>> updateResult = runDefault $ updateDocument collectionName (Just False)  docKey  (Just False) (Just False) (Just docRev) user1
-- >>> fmap isRight updateResult
-- True
-- >>> dropCollectionResult = runDefault $ dropCollection collectionName
-- >>> fmap isRight dropCollectionResult
-- True