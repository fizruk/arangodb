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
import           Data.Aeson.WithField  (OnlyField (..))
import           Data.Proxy
import           Servant.API

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XFlexibleInstances
-- >>> :set -XDeriveGeneric

type GetDocument a
  = "document"
 :> Capture "collection-name" CollectionName
 :> Capture "document-key" DocumentKey
 :> Get '[JSON] (Document a)

-- | getDocument by the key
-- >> runDefault $ getDocument "example" "key"
-- >>
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
type ReturnNew   = Maybe Bool

-- | drop non existing document
-- >> runDefault $ dropDocument "example" "key" (Just False) (Just False) (Just False) (Just (DocumentRevision "1"))
-- >>Left (FailureResponse (Response {responseStatusCode = Status {statusCode = 404, statusMessage = "Not Found"}, responseBody = "{\"error\":true,\"errorMessage\":\"document not found\",\"code\":404,\"errorNum\":1202}", responseHeaders = fromList [("X-Content-Type-Options","nosniff"),("Server","ArangoDB"),("Connection","Keep-Alive"),("Content-Type","application/json; charset=utf-8"),("Content-Length","77")], responseHttpVersion = HTTP/1.1}))

dropDocument :: CollectionName
               -> DocumentKey
               -> WaitForSync
               -> ReturnOld
               -> Silent
               -> IfMatch
               -> ArangoClientM DeleteDocumentResponse
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

updateDocument :: forall a. FromJSON a =>
            CollectionName
            -> DocumentKey
            -> WaitForSync
            -> ReturnOld
            -> Silent
            -> IfMatch
            -> ArangoClientM (Document a)
updateDocument = arangoClient (Proxy @(UpdateDocument a))

type CreateDocument a
  = "document"
 :> Capture "collection-name" CollectionName
 :> QueryParam "waitForSync" Bool
 :> QueryParam "returnNew" Bool
 :> QueryParam "silent" Bool
 :> ReqBody '[JSON] a
 :> Post '[JSON] CreateDocumentResponse

-- | test creating document
-- >> import Data.Aeson
-- >> import GHC.Generics
-- >> import Data.Either (isRight)
-- >> data Person = Person { name :: String } deriving Generic
-- >> instance ToJSON Person
-- >> instance FromJSON Person
-- >> user = Person "Nick"
-- >> result = runDefault $ createDocument "example" (Just False) (Just False) (Just False) user
-- >> fmap isRight result
-- >> True
createDocument :: forall a. (ToJSON a, FromJSON a) =>
            CollectionName
            -> WaitForSync
            -> ReturnNew
            -> Silent
            -> a
            -> ArangoClientM CreateDocumentResponse
createDocument = arangoClient (Proxy @(CreateDocument a))
