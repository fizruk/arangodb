{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
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
import qualified Data.String.Interpolate.IsString as Interpolate
import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
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
  :> ReqBody '[JSON] (CursorRequest a)
  :> Post '[JSON] (CursorResponse a)

newtype CursorId = CursorId Text
  deriving (IsString, Eq, Show, ToJSON, FromJSON)

data CursorRequest a = CursorRequest
  { query     :: Text
  , count     :: Bool
  , batchSize :: Maybe Int
  } deriving (Eq, Show, Generic)

defaultCursorRequest :: Text -> CursorRequest a
defaultCursorRequest aqlText = CursorRequest
  { query = aqlText
  , count = False
  , batchSize = Nothing
  }

instance IsString (CursorRequest a) where
  fromString = defaultCursorRequest . fromString

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
  => CursorRequest a -> ArangoClientM (CursorResponse a)
cursor = arangoClient (Proxy @(Cursor a))

instance ToJSON (CursorRequest a) where
  toJSON = genericToJSON (aesonOptions "CursorRequest")

deriveJSON' ''CursorResponse

aqlQExp :: String -> Q Exp
aqlQExp aqlString =
  [e| defaultCursorRequest $(quoteExp Interpolate.i aqlString) |]

-- | A 'QuasiQuoter' that allows you to write arbitrary AQL queries.
--
-- >>> :set -XQuasiQuotes
-- >>> :{
-- [aql|
--   FOR doc IN users
--       RETURN doc
-- |]
-- :}
-- CursorRequest {query = "\n   FOR doc IN users\n       RETURN doc\n ", count = False, batchSize = Nothing}
aql :: QuasiQuoter
aql = QuasiQuoter
  { quoteExp  = aqlQExp
  , quotePat  = error "aql QuasiQuoter used in pattern"
  , quoteType = error "aql QuasiQuoter used in type"
  , quoteDec  = error "aql QuasiQuoter used in declaration"
  }
