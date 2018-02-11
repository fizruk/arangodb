module ArangoDB.Utils.Aeson where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import ArangoDB.Utils.Naming (nameModifier)
import Language.Haskell.TH (Name, Q, Dec, nameBase)

-- | Options to derive @'ToJSON'@/@'FromJSON'@ instances.
--
-- @aesonOptions prefix@ drops @prefix@ for every field and converts
-- what's left to @snake_case@.
aesonOptions :: String -> Options
aesonOptions prefix = defaultOptions
  { fieldLabelModifier      = nameModifier prefix
  , constructorTagModifier  = nameModifier prefix
  , sumEncoding             = ObjectWithSingleField
  }

-- | Derive 'ToJSON' and 'FromJSON' with Template Haskell using 'aesonOptions'.
deriveJSON' :: Name -> Q [Dec]
deriveJSON' typeName = deriveJSON (aesonOptions (nameBase typeName)) typeName

