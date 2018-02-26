# arangodb

[![Build Status](https://travis-ci.org/fizruk/arangodb.svg?branch=master)](https://travis-ci.org/fizruk/arangodb)

ArangoDB HTTP API bindings.

## Try with GHCi

It's pretty easy to try this library from GHCi.
First run GHCi with `stack`:

```
stack ghci --package pretty-show
```

If you have a ArangoDB installed as default, you can simply use `runDefault`
to directly execute requests. Here's a sample session demonstrating collection manipulation:

```
>>> runDefault $ createCollection "example"
Right
  (WithFields
     CollectionInfo
       { collectionId = "578799"
       , collectionName = "example"
       , collectionStatus = CollectionUnloaded
       , collectionType = DocumentCollection
       , collectionIsSystem = False
       }
     CollectionProperties
       { collectionWaitForSync = False
       , collectionDoCompact = True
       , collectionJournalSize = 33554432
       , collectionKeyOptions =
           CollectionKeyOptions
             { collectionKeyType = KeyGeneratorTraditional
             , collectionKeyAllowUserKeys = True
             }
       , collectionIsVolatile = False
       , collectionNumberOfShards = Nothing
       , collectionShardKeys = Nothing
       , collectionReplicationFactor = Nothing
       })

>>> runDefault $ getCollection "example"
Right
  CollectionInfo
    { collectionId = "578799"
    , collectionName = "example"
    , collectionStatus = CollectionUnloaded
    , collectionType = DocumentCollection
    , collectionIsSystem = False
    }

>>> runDefault $ getCollectionProperties' "example"
Right
  CollectionProperties
    { collectionWaitForSync = False
    , collectionDoCompact = True
    , collectionJournalSize = 33554432
    , collectionKeyOptions =
        CollectionKeyOptions
          { collectionKeyType = KeyGeneratorTraditional
          , collectionKeyAllowUserKeys = True
          }
    , collectionIsVolatile = False
    , collectionNumberOfShards = Nothing
    , collectionShardKeys = Nothing
    , collectionReplicationFactor = Nothing
    }

>>> runDefault $ getCollectionCount' "example"
Right 0

>>> runDefault $ truncateCollection "example"
Right
  CollectionInfo
    { collectionId = "578799"
    , collectionName = "example"
    , collectionStatus = CollectionUnloaded
    , collectionType = DocumentCollection
    , collectionIsSystem = False
    }

>>> runDefault getAllNonSystemCollections
Right
  [ CollectionInfo
      { collectionId = "578799"
      , collectionName = "example"
      , collectionStatus = CollectionUnloaded
      , collectionType = DocumentCollection
      , collectionIsSystem = False
      }
  ]
  
>>> runDefault $ dropCollection "example"
Right "578799"
```
