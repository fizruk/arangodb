{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module ArangoDB.DocumentsSpec where

import ArangoDB.Utils.Client (runDefault)
import ArangoDB.Collections
import ArangoDB.Documents
import ArangoDB.Types

import Control.Applicative

import Data.Aeson
import Data.Aeson.WithField
import Data.Either (isRight)
import Data.String (fromString)

import GHC.Generics (Generic)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit (assertEqual)
import Test.QuickCheck

data NonSystemCollectionName = NonSystemCollectionName String
  deriving (Eq, Show)

-- | See <https://docs.arangodb.com/3.3/Manual/DataModeling/NamingConventions/CollectionNames.html>.
instance Arbitrary NonSystemCollectionName where
  arbitrary = NonSystemCollectionName <$>
    liftA2 (:) letter (listOf symbol) `suchThat` ((<= 64) . length)
    where
      letter = elements letters
      symbol = elements allowed

      allowed  = '-' : '_' : (letters ++ numbers)
      letters  = ['a'..'z'] ++ ['A'..'Z']
      numbers  = ['0'..'9']

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary

-- FIXME: create one collection for all document tests (see beforeAll and afterAll)
spec :: Spec
spec = do
  describe "ArangoDB HTTP API / Documents" $ do
    prop "create & drop Person document" $ \(NonSystemCollectionName name) person waitForSync -> do
      createResponse <- runDefault $ createCollection (fromString name)
      createResponse `shouldSatisfy` isRight

      createPersonResponse <- runDefault $
        createDocumentReturnNew (fromString name) (person :: Person) waitForSync
      createPersonResponse `shouldSatisfy` isRight

      let Right key = documentKey <$> createPersonResponse

      dropPersonResponse <- runDefault $
        dropDocument (fromString name) key Nothing Nothing Nothing Nothing
      dropPersonResponse `shouldSatisfy` isRight

      dropResponse   <- runDefault $ dropCollection (fromString name)
      dropResponse   `shouldSatisfy` isRight

      assertEqual "" (Right person) (extractNew <$> createPersonResponse)

  where
    extractNew = documentValue . unOnlyField . documentValue
