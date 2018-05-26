module ArangoDB.Documents (
  -- * Documents

  -- ** Read document
  getDocument,
  getDocumentById,

  -- ** Create document
  createDocument,

  -- ** Update document
  updateDocument,

  -- ** Drop document
  dropDocument,
  dropDocument_,

  -- *** Methods with extra parameters
  dropDocument',
  dropDocumentReturnOld',
  dropDocumentSilent',

) where

import           ArangoDB.Internal.Documents

