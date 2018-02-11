{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- See <https://docs.arangodb.com/3.3/HTTP/Collection/>.
module ArangoDB.Collections where

import Data.Proxy
import Servant.API
import ArangoDB.Types
import ArangoDB.Utils.Client

type GetCollection
  = "collection"
 :> Capture "collection-name" CollectionName
 :> Get '[JSON] Collection

getCollection :: CollectionName -> ArangoClientM Collection
getCollection = arangoClient (Proxy @GetCollection)

