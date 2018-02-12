# arangodb

ArangoDB HTTP API bindings.

## Try with GHCi

It's pretty easy to try this library from GHCi.
First run GHCi with `stack`:

```
stack ghci
```

If you have a ArangoDB installed as default, you can simply use `runDefault`
to directly execute requests:

```
>>> runDefault $ getCollection "test"
Right (Collection {collectionId = CollectionId "41632", collectionName = CollectionName "test", collectionStatus = CollectionLoaded})
```
