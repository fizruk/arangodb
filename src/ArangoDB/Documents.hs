{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- See <https://docs.arangodb.com/3.3/HTTP/Document/>.
module ArangoDB.Documents where

import           ArangoDB.Types
import           ArangoDB.Utils.Client
import           Data.Aeson            (FromJSON)
import           Data.Aeson.WithField  (OnlyField (..))
import           Data.Proxy
import           Servant.API

type GetDocument a
  = "document"
 :> Capture "collection-name" CollectionName
 :> Capture "document-key" DocumentKey
 :> Get '[JSON] (Document a)

getDocument :: forall a. FromJSON a => CollectionName -> DocumentKey -> ArangoClientM (Document a)
getDocument = arangoClient (Proxy @(GetDocument a))

getDocumentById :: FromJSON a => DocumentId -> ArangoClientM (Document a)
getDocumentById = uncurry getDocument . splitDocumentId

type DropDocument
  = "document"
 :> Capture "collection-name" CollectionName
 :> Capture "document-key" DocumentKey
 :> QueryParam "waitForSync" Bool
 :> QueryParam "returnOld" Bool
 :> QueryParam "silent" Bool
 :> Header "If-Match" DocumentRevision
 :> Delete '[JSON] (Document (OnlyField "old" (Maybe DocumentRevision)))


type WaitForSync = Maybe Bool
type ReturnOld   = Maybe Bool
type Silent      = Maybe Bool
type IfMatch     = Maybe DocumentRevision

dropDocument :: CollectionName
               -> DocumentKey
               -> WaitForSync
               -> ReturnOld
               -> Silent
               -> IfMatch
               -> ArangoClientM DeleteDocumentResponse
dropDocument = arangoClient (Proxy @DropDocument)

type PutDocument a
    = "document"
    :> Capture "collection-name" CollectionName
    :> Capture "document-key" DocumentKey
    :> QueryParam "waitForSync" Bool
    :> QueryParam "returnOld" Bool
    :> QueryParam "silent" Bool
    :> Header "If-Match" DocumentRevision
    :> Put '[JSON] (Document a)

putDocument :: forall a. FromJSON a =>
            CollectionName
            -> DocumentKey
            -> WaitForSync
            -> ReturnOld
            -> Silent
            -> IfMatch
            -> ArangoClientM (Document a)
putDocument = arangoClient (Proxy @(PutDocument a))
