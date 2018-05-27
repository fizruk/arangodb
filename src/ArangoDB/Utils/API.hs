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
import           GHC.TypeLits
import           Servant.API
import           Servant.Client.Core

data param :==: (value :: k)
infix 5 :==:

type family KnownType k where
  KnownType Symbol = String
  KnownType (f a)  = f (KnownType a)
  KnownType a      = a

class KnownValue (x :: k) where
  knownValue :: Proxy x -> KnownType k

instance KnownValue 'True  where knownValue _ = True
instance KnownValue 'False where knownValue _ = False

instance KnownValue 'Nothing where knownValue _ = Nothing
instance KnownValue x => KnownValue ('Just x) where
  knownValue _ = Just (knownValue (Proxy @x))

instance KnownSymbol s => KnownValue (s :: Symbol) where
  knownValue = symbolVal

instance
  ( HasClient m (param :> api)
  , Client m (param :> api) ~ (KnownType k -> Client m api)
  , KnownValue value
  ) => HasClient m ((param :==: (value :: k)) :> api) where

  type Client m ((param :==: value) :> api) = Client m api

  clientWithRoute m _api req = clientWithRoute m api' req value
    where
      api' = Proxy @(param :> api)
      value = knownValue (Proxy @value)

