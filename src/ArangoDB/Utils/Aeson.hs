{-# LANGUAGE ScopedTypeVariables #-}
module ArangoDB.Utils.Aeson where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (Parser)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import ArangoDB.Utils.Naming (nameModifier)
import Language.Haskell.TH (Name, Q, Dec, nameBase)

pPrintJSON :: ToJSON a => a -> IO ()
pPrintJSON = BSL8.putStrLn . encodePretty

-- | Options to derive @'ToJSON'@/@'FromJSON'@ instances.
--
-- @aesonOptions prefix@ drops @prefix@ for every field and converts
-- what's left to @lowerCamelCase@.
aesonOptions :: String -> Options
aesonOptions prefix = defaultOptions
  { fieldLabelModifier      = nameModifier prefix
  , constructorTagModifier  = nameModifier prefix
  , sumEncoding             = ObjectWithSingleField
  , omitNothingFields       = True
  }

-- | Derive 'ToJSON' and 'FromJSON' with Template Haskell using 'aesonOptions'.
deriveJSON' :: Name -> Q [Dec]
deriveJSON' typeName = deriveJSON (aesonOptions (nameBase typeName)) typeName

-- | Convert a value to JSON 'Value' using 'Enum'.
--
-- >>> encode (toJSONEnum True)
-- "1"
toJSONEnum :: Enum a => a -> Value
toJSONEnum = toJSON . fromEnum

parseJSONBoundedEnum
  :: forall a. (Bounded a, Enum a)
  => String             -- ^ Value description to be printed in error messages.
  -> Value -> Parser a
parseJSONBoundedEnum err js = do
  n <- parseJSON js
  if minN <= n && n <= maxN
    then return (toEnum n)
    else fail (errorMessage n)
  where
    minN = fromEnum (minBound :: a)
    maxN = fromEnum (maxBound :: a)

    errorMessage n = concat
      [ "illegal " ++ err ++ ": " ++ show n
      , " (expected a value between " ++ show minN ++ " and " ++ show maxN ++ ")"
      ]
