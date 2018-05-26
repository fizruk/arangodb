{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
module ArangoDB.Utils.API where

import           Data.Proxy
import           Servant.API
import           Servant.Client.Core

data param :==: (value :: k)
infix 5 :==:

class KnownValue (x :: a) where
  knownValue :: Proxy x -> a

instance KnownValue 'True  where knownValue _ = True
instance KnownValue 'False where knownValue _ = False

instance KnownValue 'Nothing where knownValue _ = Nothing
instance KnownValue x => KnownValue ('Just x) where
  knownValue _ = Just (knownValue (Proxy @x))

instance
  ( HasClient m (param :> api)
  , Client m (param :> api) ~ (a -> Client m api)
  , KnownValue value
  ) => HasClient m ((param :==: (value :: a)) :> api) where

  type Client m ((param :==: value) :> api) = Client m api

  clientWithRoute m _api req = clientWithRoute m api' req value
    where
      api' = Proxy @(param :> api)
      value = knownValue (Proxy @value)

