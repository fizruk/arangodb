{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
module ArangoDB.Internal.AQL where

import           ArangoDB.Utils.Aeson
import           ArangoDB.Utils.Client
import           Data.Aeson
import           Data.Proxy
import           Data.String
import           Data.Text             (Text)
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

type Cursor a
  = "cursor"
  :> ReqBody '[JSON] CursorRequest
  :> Post '[JSON] (CursorResponse a)

newtype CursorId = CursorId Text
  deriving (IsString, Eq, Show, ToJSON, FromJSON)

data CursorRequest = CursorRequest
  { query     :: Text
  , count     :: Bool
  , batchSize :: Maybe Int
  } deriving (Eq, Show)

instance IsString CursorRequest where
  fromString s = CursorRequest
    { query = fromString s
    , count = False
    , batchSize = Nothing
    }

data CursorResponse a = CursorResponse
  { hasMore :: Bool
  , id      :: Maybe CursorId
  , count   :: Maybe Int
  , result  :: [a]
  } deriving (Eq, Show)

-- | Create a query cursor (start executing).
--
-- >>> runDefault_ $ createCollection "hs_cursors"
-- >>> runDefault_ $ createDocument "hs_cursors" (Person "Nick" "K.")
-- >>> runDefaultPretty $ cursor @Person "FOR person in hs_cursors RETURN person"
-- Right
--   CursorResponse
--     { hasMore = False
--     , id = Nothing
--     , count = Nothing
--     , result = [ Person { firstname = "Nick" , lastname = "K." } ]
--     }
-- >>> runDefault_ $ dropCollection "hs_cursors"
cursor
  :: forall a. FromJSON a
  => CursorRequest -> ArangoClientM (CursorResponse a)
cursor = arangoClient (Proxy @(Cursor a))

deriveJSON' ''CursorRequest
deriveJSON' ''CursorResponse
