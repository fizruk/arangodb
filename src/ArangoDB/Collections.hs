module ArangoDB.Collections (
  -- * Collections

  -- ** Create collection
  createCollection,

  -- ** Drop collection
  dropCollection,
  dropSystemCollection,

  -- ** Truncate collections
  truncateCollection,

  -- ** Get information about collections
  getCollection,
  getCollectionProperties,
  getCollectionProperties',
  getCollectionCount,
  getCollectionCount',

  -- *** Read all collections
  getAllCollections,
  getAllNonSystemCollections,

  -- ** Modify collections
) where

import           ArangoDB.Internal.Collections

