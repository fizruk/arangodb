{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- See <https://docs.arangodb.com/3.3/HTTP/Document/>.
module ArangoDB.Documents where

import ArangoDB.Types
import ArangoDB.Utils.Client
import Data.Aeson            (FromJSON, ToJSON)
import Data.Aeson.Unit
import Data.Aeson.WithField
import Data.Proxy
import Servant.API
import Servant.Client (HasClient)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XFlexibleInstances
-- >>> :set -XDeriveGeneric

-- * Read document

type GetDocument a
  = "document"
 :> Capture "collection-name" CollectionName
 :> Capture "document-key" DocumentKey
 :> Get '[JSON] (Document a)

-- | Get document from a collection by its key.
--
getDocument
  :: forall a. (FromJSON a, HasObjectEncoding a)
  => CollectionName
  -> DocumentKey
  -> ArangoClientM (Document a)
getDocument = arangoClient (Proxy @(GetDocument a))

getDocumentById
  :: (FromJSON a, HasObjectEncoding a)
  => DocumentId
  -> ArangoClientM (Document a)
getDocumentById = uncurry getDocument . splitDocumentId

type DropDocument
  = "document"
 :> Capture "collection-name" CollectionName
 :> Capture "document-key" DocumentKey
 :> QueryParam "waitForSync" Bool
 :> QueryParam "returnOld" Bool
 :> QueryParam "silent" Bool
 :> Header "If-Match" DocumentRevision
 :> Delete '[JSON] DropDocumentResponse


type WaitForSync = Maybe Bool
type ReturnOld   = Maybe Bool
type Silent      = Maybe Bool
type IfMatch     = Maybe DocumentRevision
type ReturnNew   = Maybe Bool

dropDocument :: CollectionName
               -> DocumentKey
               -> WaitForSync
               -> ReturnOld
               -> Silent
               -> IfMatch
               -> ArangoClientM DropDocumentResponse
dropDocument = arangoClient (Proxy @DropDocument)

type UpdateDocument a
    = "document"
    :> Capture "collection-name" CollectionName
    :> Capture "document-key" DocumentKey
    :> QueryParam "waitForSync" Bool
    :> QueryParam "returnOld" Bool
    :> QueryParam "silent" Bool
    :> Header "If-Match" DocumentRevision
    :> Put '[JSON] (Document a)

updateDocument :: forall a. (FromJSON a, HasObjectEncoding a) =>
            CollectionName
            -> DocumentKey
            -> WaitForSync
            -> ReturnOld
            -> Silent
            -> IfMatch
            -> ArangoClientM (Document a)
updateDocument = arangoClient (Proxy @(UpdateDocument a))

-- | A general endpoint to create a document.
type CreateDocument a response
  = "document"
 :> QueryParam "returnNew" Bool
 :> QueryParam "silent" Bool
 :> Capture "collection-name" CollectionName
 :> ReqBody '[JSON] a
 :> QueryParam "waitForSync" Bool
 :> Post '[JSON] response

-- | A general helper to create document in different ways.
createDocument'
  :: forall a response.
    (ToJSON a, FromJSON response, HasObjectEncoding response
    , HasClient ArangoClientM (Post '[JSON] response))
  => ReturnNew
  -> Silent
  -> CollectionName
  -> a
  -> WaitForSync
  -> ArangoClientM response
createDocument' = arangoClient (Proxy @(CreateDocument a response))

-- | Create a new document and return its metadata as @'Document' 'Unit'@.
createDocument
  :: ToJSON a
  => CollectionName -> a -> WaitForSync -> ArangoClientM (Document Unit)
createDocument = createDocument' Nothing Nothing

-- | Create a new document and return an empty object (to save network traffic).
createDocumentSilent
  :: ToJSON a
  => CollectionName -> a -> WaitForSync -> ArangoClientM Unit
createDocumentSilent = createDocument' Nothing (Just True)

-- | Create a new document and return the newly created document.
createDocumentReturnNew
  :: (ToJSON a, FromJSON a, HasObjectEncoding a)
  => CollectionName
  -> a
  -> WaitForSync
  -> ArangoClientM (Document (OnlyField "new" (Document a)))
createDocumentReturnNew = createDocument' (Just True) Nothing
