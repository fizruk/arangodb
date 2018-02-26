{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module ArangoDB.Utils.Enum where

import Generics.SOP

gfromEnum :: IsEnumType a => a -> Int
gfromEnum = conIndex . unSOP . from
  where
    conIndex :: NS f xs -> Int
    conIndex (Z _) = 0
    conIndex (S i) = 1 + conIndex i

gtoEnum :: IsEnumType a => Int -> a
gtoEnum i =
  to (SOP (apInjs_NP (hcpure (Proxy @ ((~) '[])) Nil) !! i))
