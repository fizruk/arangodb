{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module ArangoDB.Documents where

import Data.Aeson (FromJSON)
import Data.Proxy
import Servant.API
import ArangoDB.Types
import ArangoDB.Utils.Client

type GetDocument a
  = "document"
 :> Capture "collection-name" CollectionName
 :> Capture "document-key" DocumentKey
 :> Get '[JSON] (Document a)

getDocument :: forall a. FromJSON a => CollectionName -> DocumentKey -> ArangoClientM (Document a)
getDocument = arangoClient (Proxy @(GetDocument a))

getDocumentById :: FromJSON a => DocumentId -> ArangoClientM (Document a)
getDocumentById = uncurry getDocument . splitDocumentId
