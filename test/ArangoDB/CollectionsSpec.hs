{-# LANGUAGE OverloadedStrings #-}
module ArangoDB.CollectionsSpec where

import ArangoDB.Utils.Client (runDefault)
import ArangoDB.Collections

import Control.Applicative

import Data.Either (isRight)
import Data.String (fromString)

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

spec :: Spec
spec = do
  describe "ArangoDB HTTP API / Collections" $ do
    prop "create, get & drop collection" $ \(NonSystemCollectionName name) -> do
      createResponse <- runDefault $ createCollection (fromString name)
      createResponse `shouldSatisfy` isRight

      getResponse <- runDefault $ getCollectionProperties (fromString name)
      getResponse `shouldSatisfy` isRight

      dropResponse   <- runDefault $ dropCollection (fromString name)
      dropResponse   `shouldSatisfy` isRight

      assertEqual "" createResponse getResponse
