{-# LANGUAGE ScopedTypeVariables #-}
module ArangoDB.Utils.Aeson where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.WithField (OnlyField (..))
import qualified Data.ByteString.Lazy.Char8 as BSL8
import ArangoDB.Utils.Naming (nameModifier)
import Language.Haskell.TH (Name, Q, Dec, nameBase)

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

-- | Convert JSON 'Value' into JSON 'Object'.
--
-- If conversion is impossible a runtime error occurs:
--
-- >>> unsafeToJSONObject 123
-- fromList *** Exception:
-- Expected JSON object, but got JSON number:
--   123
-- <BLANKLINE>
-- Hint: use OnlyField to turn your value into an object with a single field.
-- For instance:
-- <BLANKLINE>
--   import Data.Aeson (encode)
--   import Data.Aeson.WithField (OnlyField(..))
-- <BLANKLINE>
--   example :: OnlyField "result" Value
--   example = OnlyField (Number 123.0)
-- <BLANKLINE>
-- ... >>> encode example
-- ... "{\"result\":123}"
-- ...
unsafeToJSONObject :: ToJSON a => a -> Object
unsafeToJSONObject x = case toJSON x of
  Object o -> o
  value -> error $ unlines
    [ ""
    , "Expected JSON object, but got " ++ jsonType value ++ ":"
    , "  " ++ BSL8.unpack (encode value)
    , ""
    , "Hint: use OnlyField to turn your value into an object with a single field."
    , "For instance:"
    , ""
    , "  import Data.Aeson (encode)"
    , "  import Data.Aeson.WithField (OnlyField(..))"
    , ""
    , "  example :: OnlyField \"result\" Value"
    , "  example = OnlyField (" ++ show value ++ ")"
    , ""
    , "  >>> encode example"
    , "  " ++ show (encode (OnlyField value :: OnlyField "result" Value))
    ]
  where
    jsonType (Array  _) = "JSON array"
    jsonType (String _) = "JSON string"
    jsonType (Number _) = "JSON number"
    jsonType (Bool   _) = "JSON boolean value"
    jsonType (Object _) = "JSON object"
    jsonType Null       = "JSON null"

